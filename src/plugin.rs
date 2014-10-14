use std::collections::TreeMap;

use syntax::ast;
use syntax::codemap;
use syntax::codemap::DUMMY_SP;
use syntax::ext::build::AstBuilder;
use syntax::ext::base::{ExtCtxt, MacExpr, MacResult};
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::ptr::P;

use parse::parse_scan_arm;
use parse::{ScanArm, FallbackArm, PatternArm, ScanAttrs};
use parse::PatAst;

use scan_util::{Tokenizer, Whitespace};

pub fn expand_scan(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	debug!("expand_scan(cx, sp, tts)");
	let mut p = cx.new_parser_from_tts(tts);

	let setup_stmts = vec![];

	let input_expr = p.parse_expr();
	p.expect(&token::COMMA);

	let input_expr = quote_expr!(cx, {
		use std::str::Str;

		rt::Ok(($input_expr).as_slice())
	});

	let (arms, fallback) = parse_scan_body(cx, &mut p, sp);

	debug!("expand_scan - making scan expression");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback);
	MacExpr::new(scan_expr)
}

pub fn expand_scanln(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	debug!("expand_scanln(cx, sp, tts)");
	let mut p = cx.new_parser_from_tts(tts);

	let setup_stmts = vec![
		quote_stmt!(cx, let line = rt::io::stdin_read_line();),
		quote_stmt!(cx, let line_str = match &line {
			&Err(ref err) => Err(rt::ScanIoError(err.clone())),
			&Ok(ref line) => Ok(line.as_slice().trim_right_chars('\n').trim_right_chars('\r'))
		};),
	];

	let input_expr = quote_expr!(cx, line_str);

	let (arms, fallback) = parse_scan_body(cx, &mut p, sp);

	debug!("expand_scanln - making scan expression...");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback);
	debug!("expand_scanln - scan_expr: {}", scan_expr);
	MacExpr::new(scan_expr)
}

fn parse_scan_body(cx: &mut ExtCtxt, p: &mut Parser, sp: codemap::Span) -> (Vec<(ScanArm, P<ast::Expr>)>, Option<(ScanArm, P<ast::Expr>)>) {
	let mut arms = vec![];
	let mut fallback = None;

	while p.token != token::EOF && fallback.is_none() {
		debug!("parse_scan_body - parsing scan arm");
		match parse_scan_arm(cx, p) {
			arm @ (FallbackArm(_), _) => fallback = Some(arm),
			arm @ (PatternArm(..), _) => arms.push(arm),
		}
	}

	if fallback.is_some() && p.token != token::EOF {
		debug!("parse_scan_body - got something after fallback");
		let sp = p.span;
		let tok_str = p.this_token_to_string();
		p.span_note(sp, format!("there shouldn't be anything after the fallback arm, found `{}`", tok_str).as_slice());
		p.unexpected();
	}

	if arms.len() == 0 && fallback.is_none() {
		p.span_fatal(sp, "expected at least one scan arm");
	}

	(arms, fallback)
}

fn make_scan_expr(cx: &mut ExtCtxt, setup_stmts: Vec<P<ast::Stmt>>, input_expr: P<ast::Expr>, arms: Vec<(ScanArm, P<ast::Expr>)>, fallback_arm: Option<(ScanArm, P<ast::Expr>)>) -> P<ast::Expr> {
	debug!("make_scan_expr(...)");

	debug!("make_scan_expr - generating module setup statement...");
	let mod_setup_stmt = quote_stmt!(cx,
		mod rt {
			extern crate scan_util;
			pub use std::option::{None, Some};
			pub use std::result::{Result, Err, Ok};
			pub use std::vec::Vec;
			pub use self::scan_util::{
				Cursor, ScanCursor,
				io,
				tokenizer,
				whitespace,
				ScanError, NothingMatched, OtherScanError, ScanIoError,
				Scanner,
			};
		});

	let mut scan_arm_stmts = vec![];

	debug!("make_scan_expr - generating scan arms...")
	for (i,arm) in arms.into_iter().enumerate() {
		scan_arm_stmts.push(gen_arm_stmt(cx, arm, i == 0))
	}

	match fallback_arm {
		None => (),
		Some(arm) => {
			debug!("make_scan_expr - generating scan fallback arm...")
			let is_first = scan_arm_stmts.len() == 0;
			scan_arm_stmts.push(gen_arm_stmt(cx, arm, is_first))
		}
	}

	debug!("make_scan_expr - generating error arm");
	let err_arm = cx.arm(DUMMY_SP,
		vec![quote_pat!(cx, rt::Err(err))],
		quote_expr!(cx, rt::Err(err)),
	);

	debug!("make_scan_expr - generating ok arm");
	let ok_arm = cx.arm(
		DUMMY_SP,
		vec![quote_pat!(cx, rt::Ok(input))],
		/*quote_expr!(cx, {
			let cur = rt::Cursor::new(
				input,
				rt::tokenizer::WordsAndInts,
				rt::whitespace::Ignore);
			//let mut result = rt::Err(rt::NothingMatched);
			let mut result: rt::Result<_, rt::ScanError>;

			$scan_arm_stmts

			result
		})*/
		cx.expr_block(
			cx.block(DUMMY_SP,
				/*stmts:*/{
					let mut stmts = vec![
						quote_stmt!(cx,
							let mut result: rt::Result<_, rt::ScanError>;
						),
					];
					stmts.extend(scan_arm_stmts.into_iter());
					stmts
				},
				/*expr:*/Some(cx.expr_ident(DUMMY_SP,
					cx.ident_of("result")
				))
			)
		)
	);

	debug!("make_scan_expr - building match expr");
	let match_expr = cx.expr_match(DUMMY_SP,
		input_expr,
		vec![err_arm, ok_arm],
	);

	debug!("make_scan_expr - building block expr");
	/*let expr = quote_expr!(cx, {
		use scan_util::Scanner;
		use scan_util::ScanCursor;
		$mod_setup_stmt
		$setup_stmts
		$match_expr
	});*/
	let expr = cx.expr_block(
		cx.block_all(DUMMY_SP,
			/*view_items:*/vec![
				// This kinda sucks, but I can't find a way around this.  This requires the user to add an explicit `extern crate scan_util;` to their root module.
				cx.view_use_simple(DUMMY_SP,
					ast::Inherited,
					cx.path_global(DUMMY_SP,
						vec![
							cx.ident_of("scan_util"),
							cx.ident_of("Scanner"),
						]
					)
				),
				cx.view_use_simple(DUMMY_SP,
					ast::Inherited,
					cx.path_global(DUMMY_SP,
						vec![
							cx.ident_of("scan_util"),
							cx.ident_of("ScanCursor"),
						]
					)
				),
			],
			/*stmts:*/{
				let mut stmts = vec![mod_setup_stmt];
				stmts.extend(setup_stmts.into_iter());
				stmts
			},
			/*expr:*/Some(match_expr)
		)
	);

	expr
}

fn gen_arm_stmt(cx: &mut ExtCtxt, arm: (ScanArm, P<ast::Expr>), is_first: bool) -> P<ast::Stmt> {
	debug!("gen_arm_stmt(cx, {}, {})", arm, is_first);
	let (arm_pat, arm_expr) = arm;

	// We need to wrap arm_expr in a block and a partially constrained Ok.  If we don't wrap it in a block, `break` won't work---the compiler will complain about an unconstrained type.
	/*let arm_expr = quote_expr!(cx, rt::Ok::<_, rt::ScanError>({ $arm_expr }));*/
	let arm_expr = cx.expr_call(DUMMY_SP,
		quote_expr!(cx, rt::Ok::<_, rt::ScanError>),
		vec![
			cx.expr_block(cx.block_expr(arm_expr)),
		]
	);

	let (cur_stmt, arm_expr) = match arm_pat {
		PatternArm(pattern, tail, attrs) => {
			let tok = cx.expr_path(cx.path(DUMMY_SP,
				attrs.inp_tok.as_slice().split_str("::").map(|s| cx.ident_of(s)).collect()
			));
			let sp = quote_expr!(cx, rt::whitespace::Ignore);
			let cur_stmt = quote_stmt!(cx,
				let cur = rt::Cursor::new(input, $tok, $sp);
			);
			let and_then = match tail {
				None => {
					/*quote_expr!(cx, {
						match cur.pop_token() {
							Some((tok, cur)) => rt::Err(
								rt::OtherScanError(
									format!("expected end of input, got `{}`", tok),
									cur.consumed()
								)
							),
							None => $arm_expr
						}
					})*/
					cx.expr_match(DUMMY_SP,
						quote_expr!(cx, cur.pop_token()),
						vec![
							cx.arm(DUMMY_SP,
								vec![quote_pat!(cx, Some((_, cur)))],
								quote_expr!(cx,
									rt::Err(cur.expected_eof())
								)
							),
							cx.arm(DUMMY_SP,
								vec![quote_pat!(cx, None)],
								arm_expr
							)
						]
					)
				},
				Some(ident) => {
					/*quote_expr!(cx, {
						let $ident = cur.tail_str();
						$arm_expr
					})*/
					cx.expr_block(
						cx.block(DUMMY_SP,
							vec![
								cx.stmt_let(
									ident.span, /*mutbl:*/false, ident.node,
									quote_expr!(cx, cur.tail_str())
								),
							],
							Some(arm_expr)
						)
					)
				}
			};
			(Some(cur_stmt), gen_ast_scan_expr(cx, &attrs, pattern, and_then))
		},
		FallbackArm(None) => {
			(None, arm_expr)
		},
		FallbackArm(Some(ident)) => {
			/*quote_expr!(cx, {
				let $ident = input;
				$arm_expr
			})*/
			(None, cx.expr_block(
				cx.block(DUMMY_SP,
					vec![
						cx.stmt_let(ident.span, /*mutbl:*/false, ident.node, quote_expr!(cx, input)),
					],
					Some(arm_expr)
				)
			))
		},
	};

	// Merge cur_stmt and arm_expr into a block.
	let arm_expr = cx.expr_block(cx.block(DUMMY_SP,
		cur_stmt.into_iter().collect(),
		Some(arm_expr)
	));

	debug!("gen_arm_stmt - done building arm_expr");

	if is_first {
		/*quote_stmt!(cx,
			result = $arm_expr;
		)*/
		cx.stmt_expr(
			cx.expr(DUMMY_SP,
				ast::ExprAssign(
					cx.expr_ident(DUMMY_SP, cx.ident_of("result")),
					arm_expr
				)
			)
		)
	} else {
		/*quote_stmt!(cx,
			result = match result {
				Err(last_err) => {
					match {
						$arm_expr
					} {
						Err(new_err) => Err(new_err.or(last_err)),
						other => other
					}
				},
				other => other
			};
		)*/
		cx.stmt_expr(
			cx.expr(DUMMY_SP,
				ast::ExprAssign(
					quote_expr!(cx, result),
					cx.expr_match(DUMMY_SP,
						quote_expr!(cx, result),
						vec![
							cx.arm(DUMMY_SP,
								/*pats:*/vec![
									//quote_pat!(cx, Err(last_err)),
									cx.pat_enum(DUMMY_SP,
										cx.path(DUMMY_SP,
											vec![
												cx.ident_of("rt"),
												cx.ident_of("Err"),
											]
										),
										/*subpats:*/vec![
											cx.pat_ident(DUMMY_SP,
												cx.ident_of("last_err")
											)
										]
									),
								],
								/*expr:*/cx.expr_match(DUMMY_SP,
									arm_expr,
									vec![
										/*quote_arm!(cx, Err(new_err) => Err(new_err.or(last_err))),*/
										cx.arm(DUMMY_SP,
											vec![
												cx.pat_enum(DUMMY_SP,
													cx.path(DUMMY_SP,
														vec![
															cx.ident_of("rt"),
															cx.ident_of("Err"),
														]
													),
													vec![
														cx.pat_ident(DUMMY_SP, cx.ident_of("new_err")),
													]
												)
											],
											quote_expr!(cx, rt::Err(new_err.or(last_err)))
										),
										/*quote_arm!(cx, other => other)*/
										cx.arm(DUMMY_SP,
											vec![
												cx.pat_ident(DUMMY_SP, cx.ident_of("other"))
											],
											quote_expr!(cx, other)
										)
									]
								)
							),
							quote_arm!(cx, other => other,)
						]
					)
				)
			)
		)
	}
}

fn gen_ast_scan_expr(cx: &mut ExtCtxt, attrs: &ScanAttrs, node: PatAst, and_then: P<ast::Expr>) -> P<ast::Expr> {
	use parse::{AstAlternates, AstSequence, AstText, AstOptional, AstCapture, AstRepetition, RepeatRange};
	debug!("gen_ast_scan_expr(cx, {}, {}, {})", attrs, node, and_then);

	let captures = enumerate_captures(&node);


	let captures = enumerate_captures(&node);

	match node {
		AstAlternates(nodes) => {
			assert!(nodes.len() > 0);

			// Construct the expression for each alternative.
			let alt_exprs: Vec<_> = nodes.into_iter().map(|node| {
				let alt_and_then = {
					// Any capture which is in this branch gets returned as `Some(v)`; any capture *not* in this branch gets returned as `None`.
					let alt_captures = enumerate_captures(&node);

					let alt_results = captures.keys().map(|&ident| {
						if alt_captures.contains_key(&ident) {
							/*quote_expr!(cx, Some($ident))*/
							cx.expr_call(DUMMY_SP,
								quote_expr!(cx, rt::Some),
								vec![
									cx.expr_ident(DUMMY_SP,
										ident
									),
								]
							)
						} else {
							quote_expr!(cx, None)
						}
					});

					// Every result starts with the cursor.
					let cur_iter = Some(quote_expr!(cx, cur)).into_iter();

					// Turn it into a tuple expr, wrap in an Ok.
					cx.expr_ok(DUMMY_SP,
						cx.expr_tuple(DUMMY_SP, cur_iter.chain(alt_results).collect())
					)
				};

				// Now generate the match expr.
				gen_ast_scan_expr(cx, attrs, node, alt_and_then)
			}).collect();

			// We now need to turn the vector of `Expr`s into a chain of `or_else` calls.
			let alt_branches = {
				/*quote_expr!(cx, {
					$alt_exprs[0]
						$( .or_else(|| $alt_exprs[1..]) )
				})*/
				let mut alt_exprs = alt_exprs.into_iter();
				let first = alt_exprs.next().unwrap();
				alt_exprs.fold(first, |lhs, rhs| {
					// quote_expr!(cx, $lhs.or_else(|| $rhs))
					cx.expr_method_call(DUMMY_SP,
						lhs,
						cx.ident_of("or_else"),
						vec![
							// TODO: combine errors properly.
							// quote_expr!(cx, |_| $rhs)
							cx.lambda_expr_1(DUMMY_SP, rhs, cx.ident_of("_")),
						]
					)
				})
			};

			// Finally, we need the capture pattern.
			let capture_pattern = cx.pat_tuple(DUMMY_SP,
				{
					let pats = captures.keys().map(|&ident| {
						cx.pat_ident(DUMMY_SP, ident)
					});
					Some(quote_pat!(cx, cur)).into_iter()
						.chain(pats)
						.collect()
				}
			);

			/*quote_expr!(cx, {
				match $alt_branches {
					Err(err) => Err(err),
					Ok($capture_pattern) => $and_then
				}
			})*/
			cx.expr_match(DUMMY_SP,
				alt_branches,
				vec![
					cx.arm(DUMMY_SP,
						vec![quote_pat!(cx, rt::Err(err))],
						quote_expr!(cx, rt::Err(err))
					),
					cx.arm(DUMMY_SP,
						vec![
							cx.pat_enum(DUMMY_SP,
								cx.path(DUMMY_SP,
									vec![
										cx.ident_of("rt"),
										cx.ident_of("Ok"),
									]
								),
								vec![capture_pattern]
							)
						],
						and_then
					),
				]
			)
		},
		AstSequence(nodes) => {
			// We need to do this *backwards* because in order to generate the AST for a node, we need to have the and_then AST.
			nodes.into_iter().rev()
				.fold(and_then, |and_then, node| gen_ast_scan_expr(cx, attrs, node, and_then))
		},
		AstText(s) => {
			use parse::{WordsAndInts, IdentsAndInts, SpaceDelimited, ExplicitTok};
			use scan_util::{Tokenizer, tokenizer};

			// TODO: allow these to be overridden.
			let tc = match attrs.pat_tok {
				WordsAndInts => box tokenizer::WordsAndInts as Box<Tokenizer>,
				IdentsAndInts => box tokenizer::IdentsAndInts as Box<Tokenizer>,
				SpaceDelimited => box tokenizer::SpaceDelimited as Box<Tokenizer>,
				ExplicitTok => box tokenizer::Explicit as Box<Tokenizer>,
			};
			let sp = ::scan_util::whitespace::Ignore;

			let mut and_then = and_then;

			for tok in text_to_tokens(s.as_slice(), &*tc, &sp).into_iter().rev() {
				let tok_expr = str_to_expr(cx, tok);
				/*and_then = quote_expr!(cx, {
					match cur.expect_tok($tok_expr) {
						rt::Err(err) => rt::Err(err),
						rt::Ok(cur) => $and_then
					}
				});*/
				and_then = cx.expr_block(
					cx.block_expr(
						cx.expr_match(DUMMY_SP,
							//quote_expr!(cx, cur.expect_tok($tok_expr)),
							cx.expr_method_call(DUMMY_SP,
								/*expr:*/cx.expr_path(
									cx.path(DUMMY_SP,
										vec![
											cx.ident_of("cur"),
										]
									)
								),
								/*ident:*/cx.ident_of("expect_tok"),
								/*args:*/vec![
									tok_expr,
								]
							),
							vec![
								//quote_arm!(cx, rt::Err(err) => rt::Err(err)),
								cx.arm(DUMMY_SP,
									/*pats:*/vec![
										cx.pat_enum(DUMMY_SP,
											cx.path(DUMMY_SP,
												vec![
													cx.ident_of("rt"),
													cx.ident_of("Err"),
												]
											),
											/*subpats:*/vec![
												cx.pat_ident(DUMMY_SP,
													cx.ident_of("err")
												)
											]
										),
									],
									/*expr:*/cx.expr_call(DUMMY_SP,
										cx.expr_path(
											cx.path(DUMMY_SP,
												vec![
													cx.ident_of("rt"),
													cx.ident_of("Err"),
												]
											)
										),
										vec![
											cx.expr_path(
												cx.path(DUMMY_SP,
													vec![
														cx.ident_of("err"),
													]
												)
											),	
										]
									)
								),
								cx.arm(DUMMY_SP,
									/*pats:*/vec![
										cx.pat_enum(DUMMY_SP,
											cx.path(DUMMY_SP,
												vec![
													cx.ident_of("rt"),
													cx.ident_of("Ok"),
												]
											),
											/*subpats:*/vec![
												cx.pat_ident(DUMMY_SP,
													cx.ident_of("cur")
												)
											]
										),
									],
									/*expr:*/and_then
								)
							]
						)
					)
				);
			}

			and_then
		},
		AstOptional(box node) => {
			// Desugars into ($node|).
			let alts = vec![node, AstSequence(vec![])];
			gen_ast_scan_expr(cx, attrs, AstAlternates(alts), and_then)
		},
		AstCapture(ident, ty) => {
			/*quote_expr!(cx, {
				match rt::Scanner::scan(&cur.pop_ws()) {
					Err(err) => Err(err),
					Ok((val, cur)) => {
						let $ident: $ty = val;

						$and_then
					}
				}
			})*/
			cx.expr_match(DUMMY_SP,
				quote_expr!(cx, rt::Scanner::scan(&cur.pop_ws())),
				vec![
					quote_arm!(cx, rt::Err(err) => rt::Err(err),),
					cx.arm(DUMMY_SP,
						vec![
							quote_pat!(cx, rt::Ok((val, cur))),
						],
						cx.expr_block(
							cx.block(DUMMY_SP,
								vec![
									/*quote_stmt!(cx,
										let $ident: $ty = val;
									),*/
									match ty {
										None => cx.stmt_let(ident.span,
											/*mutbl:*/false,
											ident.node,
											quote_expr!(cx, val)
										),
										Some(ty) => cx.stmt_let_typed(ident.span,
											/*mutbl:*/false,
											ident.node,
											ty,
											quote_expr!(cx, val)
										)
									},
								],
								Some(and_then)
							)
						)
					)
				]
			)
		},
		AstRepetition { node, sep, range } => {
			let node_captures = enumerate_captures(&*node);
			let sep_captures = sep.as_ref()
				.map(|sep| enumerate_captures(&**sep))
				.unwrap_or(TreeMap::new());

			let RepeatRange(range_min, range_max) = range;
			let range_max = range_max.unwrap_or(::std::uint::MAX);

			let range_min = cx.expr_uint(DUMMY_SP, range_min);
			let range_max = cx.expr_uint(DUMMY_SP, range_max);

			/*let node_and_then = quote_expr!(cx, {
				(cur, $(node_capture_idents),*)
			});*/
			let node_and_then = cx.expr_ok(DUMMY_SP, cx.expr_tuple(DUMMY_SP,
				Some(quote_expr!(cx, cur)).into_iter()
					.chain(node_captures.iter().map(|(&i,&(sp, _))| cx.expr_ident(sp, i)))
					.collect()
			));
			let node_scan = gen_ast_scan_expr(cx, attrs, *node, node_and_then);
			/*let node_pat = quote_pat!(cx,
				(_cur, $(node_capture_idents),*)
			);*/
			let node_pat = cx.pat_tuple(DUMMY_SP,
				Some(quote_pat!(cx, _cur)).into_iter()
					.chain(node_captures.iter().map(|(&i,&(sp, _))| cx.pat_ident(sp, i)))
					.collect()
			);
			/*let rep_result = quote_expr!(cx,
				(cur, $(vec_ $~ capture_idents),*)
			);*/
			let rep_result = cx.expr_tuple(DUMMY_SP,
				Some(quote_expr!(cx, cur)).into_iter()
					.chain(captures.iter()
						.map(|(&i,&(sp, _))|
							cx.expr_ident(sp,
								cx.ident_of(format!("vec_{}",i.as_str()).as_slice())
							)
						)
					)
					.collect()
			);
			/*let sep_and_then = quote_expr!(cx, {
				(cur, $(sep_capture_idents),*)
			});*/
			let sep_and_then = cx.expr_ok(DUMMY_SP, cx.expr_tuple(DUMMY_SP,
				Some(quote_expr!(cx, cur)).into_iter()
					.chain(sep_captures.iter().map(|(&i,&(sp, _))| cx.expr_ident(sp, i)))
					.collect()
			));
			/*let sep_pat = quote_pat!(cx,
				(_cur, $(sep_capture_idents),*)
			);*/
			let sep_pat = cx.pat_tuple(DUMMY_SP,
				Some(quote_pat!(cx, _cur)).into_iter()
					.chain(sep_captures.iter().map(|(&i,&(sp,_))| cx.pat_ident(sp, i)))
					.collect()
			);
			let sep_scan = match sep {
				None => None,
				Some(box sep) => Some(gen_ast_scan_expr(cx, attrs, sep, sep_and_then))
			};
			/*let rep_scan = quote_expr!(cx, {
				let mut cur = cur;
				let mut err = rt::None::<rt::ScanError>;
				let mut repeats = 0;
				let mut trailing_sep = false;
				$(let mut vec_ $~ $capture_idents : Vec<$capture_tys> = vec![];)*

				while repeats < $range_max {
					$if let Some(sep_scan) = sep_scan {
						if repeats > 0 {
							match $sep_scan {
								Err(_err) => {
									err = rt::Some(_err);
									break;
								},
								Ok($sep_pat) => {
									cur = _cur;
									trailing_sep = true;
									$(vec_ $~ $sep_capture_idents.push($sep_capture_idents);)*
								}
							}
						}
					}
					match $node_scan {
						Err(_err) => {
							err = rt::Some(_err);
							break;
						},
						Ok($node_pat) => {
							cur = _cur;
							repeats += 1;
							trailing_sep = false;
							$(vec_ $~ $capture_idents.push($capture_idents);)*
						}
					}
				}

				// Hack to get around not being able to mark expressions with attributes.
				#[allow(type_limits)]
				#[inline(always)]
				fn check_repeats(got: uint) -> bool { $range_min <= got }

				if check_repeats(repeats) && !trailing_sep {
					rt::Ok($rep_result)
				} else {
					match err {
						rt::None => {
							// No error, but we didn't get enough matches.
							rt::Err(cur.expected_min_repeats($range_min, repeats))
						},
						rt::Some(err) => rt::Err(err)
					}
				}
			});*/
			let rep_scan = cx.expr_block(cx.block(DUMMY_SP,
				vec![
					quote_stmt!(cx, let mut cur = cur;),
					quote_stmt!(cx, let mut err = rt::None::<rt::ScanError>;),
					quote_stmt!(cx, let mut repeats = 0u;),
					quote_stmt!(cx, let mut trailing_sep = false;),
				] + captures.iter().map(|(&ident, &(sp, _))| {
					let ty = cx.ty_infer(sp);
					cx.stmt_let_typed(
						sp,
						/*mutbl:*/true,
						cx.ident_of(format!("vec_{}",ident.as_str()).as_slice()),
						quote_ty!(cx, rt::Vec<$ty>),
						quote_expr!(cx, rt::Vec::new())
					)
				}).collect::<Vec<_>>() + vec![
					cx.stmt_expr(
						cx.expr(DUMMY_SP,
							ast::ExprWhile(
								quote_expr!(cx, repeats < $range_max),
								cx.block(DUMMY_SP,
									vec![
										match sep_scan {
											None => quote_stmt!(cx, ();),
											Some(sep_scan) => {
												cx.stmt_expr(
													cx.expr_if(DUMMY_SP,
														quote_expr!(cx, repeats > 0),
														cx.expr_match(DUMMY_SP,
															sep_scan,
															vec![
																quote_arm!(cx,
																	rt::Err(_err) => {
																		err = rt::Some(_err);
																		break;
																	},
																),
																cx.arm(DUMMY_SP,
																	vec![
																		cx.pat_ok(DUMMY_SP, sep_pat),
																	],
																	cx.expr_block(cx.block(DUMMY_SP,
																		vec![
																			quote_stmt!(cx, cur = _cur;),
																			quote_stmt!(cx, trailing_sep = true;),
																		] + sep_captures.iter().map(|(&ident, &(sp, _))| {
																			cx.stmt_expr(cx.expr_method_call(DUMMY_SP,
																				cx.expr_ident(DUMMY_SP,
																					cx.ident_of(format!("vec_{}", ident.as_str()).as_slice())
																				),
																				cx.ident_of("push"),
																				vec![
																					cx.expr_ident(sp, ident),
																				]
																			))
																		}).collect::<Vec<_>>(),
																		None
																	))
																)
															]
														),
														None
													)
												)
											}
										}
									],
									Some(cx.expr_match(DUMMY_SP,
										node_scan,
										vec![
											quote_arm!(cx,
												rt::Err(_err) => {
													err = rt::Some(_err);
													break;
												},
											),
											cx.arm(DUMMY_SP,
												vec![
													cx.pat_ok(DUMMY_SP, node_pat),
												],
												cx.expr_block(cx.block(DUMMY_SP,
													vec![
														quote_stmt!(cx, cur = _cur;),
														quote_stmt!(cx, repeats += 1;),
														quote_stmt!(cx, trailing_sep = false;),
													] + node_captures.iter().map(|(&ident, &(sp, _))| {
														cx.stmt_expr(cx.expr_method_call(DUMMY_SP,
															cx.expr_ident(DUMMY_SP,
																cx.ident_of(format!("vec_{}",ident.as_str()).as_slice())
															),
															cx.ident_of("push"),
															vec![
																cx.expr_ident(sp, ident),
															]
														))
													}).collect::<Vec<_>>(),
													None
												))
											)
										]
									))
								),
								None
							)
						)
					),
					cx.stmt_item(DUMMY_SP,
						quote_item!(cx,
							// Hack to get around not being able to mark expressions with attributes.
							#[allow(type_limits)]
							#[inline(always)]
							fn check_repeats(got: uint) -> bool { $range_min <= got }
						).unwrap()
					),
				],
				Some(quote_expr!(cx,
					if check_repeats(repeats) && !trailing_sep {
						rt::Ok($rep_result)
					} else {
						match err {
							rt::None => {
								// No error, but we didn't get enough matches.
								rt::Err(cur.expected_min_repeats($range_min, repeats))
							},
							rt::Some(err) => rt::Err(err)
						}
					}
				))
			));
			/*let rep_pat = quote_pat!(cx,
				(cur, $(captures),*)
			);*/
			let rep_pat = cx.pat_tuple(DUMMY_SP,
				Some(quote_pat!(cx, cur)).into_iter()
					.chain(captures.iter().map(|(&i,&(sp,_))| cx.pat_ident(sp, i)))
					.collect()
			);
			/*quote_expr!(cx, {
				match $rep_scan {
					Err(err) => Err(err),
					Ok($rep_pat) => $and_then
				}
			})
			*/
			cx.expr_match(DUMMY_SP,
				rep_scan,
				vec![
					quote_arm!(cx, Err(err) => Err(err),),
					cx.arm(DUMMY_SP,
						vec![
							cx.pat_ok(DUMMY_SP, rep_pat),
						],
						and_then
					)
				]
			)
		},
	}
}

// Annoyingly, Span doesn't implement Ord, hence why we have to split this up.
type CaptureMap = TreeMap<ast::Ident, (codemap::Span, Option<P<ast::Ty>>)>;

fn enumerate_captures(node: &PatAst) -> CaptureMap {
	use parse::{AstAlternates, AstSequence, AstText, AstOptional, AstCapture, AstRepetition};
	debug!("enumerate_captures(&{})", node);

	fn merge(mut lhs: CaptureMap, rhs: CaptureMap) -> CaptureMap {
		// TODO: Ensure types are the same.
		lhs.extend(rhs.into_iter());
		lhs
	}

	match node {
		&AstAlternates(ref nodes) | &AstSequence(ref nodes) => {
			nodes.iter().map(enumerate_captures).fold(TreeMap::new(), |a,b| merge(a,b))
		},
		&AstText(_) => {
			TreeMap::new()
		},
		&AstOptional(ref node) => {
			enumerate_captures(&**node)
		},
		&AstCapture(ident, ref ty) => {
			let mut set = TreeMap::new();
			set.insert(ident.node, (ident.span, ty.clone()));
			set
		},
		&AstRepetition { ref node, ref sep, range: _ } => {
			let mut captures = enumerate_captures(&**node);
			match sep {
				&Some(ref sep) => captures = merge(captures, enumerate_captures(&**sep)),
				&None => ()
			}
			captures
		},
	}
}

fn text_to_tokens<'a>(s: &'a str, tc: &Tokenizer, sp: &Whitespace) -> Vec<&'a str> {
	let mut toks = vec![];
	let mut s = s;

	while s.len() > 0 {
		match sp.token_len(s) {
			None => (),
			Some(end) => {
				toks.push(s.slice_to(end));
				s = s.slice_from(end);
				continue;
			}
		}

		s = s.slice_from(sp.strip_len(s));
		if s.len() > 0 {
			let end = tc.token_len(s).unwrap_or(1);
			toks.push(s.slice_to(end));
			s = s.slice_from(end);
		}
	}

	toks
}

fn str_to_expr<S: Str>(cx: &mut ExtCtxt, s: S) -> P<ast::Expr> {
	cx.expr_str(codemap::DUMMY_SP, token::intern_and_get_ident(s.as_slice()))
}

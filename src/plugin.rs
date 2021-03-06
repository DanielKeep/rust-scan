use std::collections::HashMap;

use syntax::ast;
use syntax::codemap;
use syntax::codemap::DUMMY_SP;
use syntax::ext::build::AstBuilder;
use syntax::ext::base::{ExtCtxt, MacExpr, MacItems, MacResult};
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::ptr::P;

use util::BoolMap;
use parse::parse_scan_arm;
use parse::{ScanArm, FallbackArm, PatternArm, ScanArmAttrs};
use parse::PatAst;
use parse::FormatSource;

use scan_util::{Tokenizer, Whitespace, CompareStrs};

use self::ScanKind::{NormalScan, ScannerScan};

#[derive(Copy, Show)]
enum ScanKind {
	NormalScan,
	ScannerScan,
}

pub fn expand_scan(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	debug!("expand_scan(cx, sp, tts)");
	let mut p = cx.new_parser_from_tts(tts);

	let setup_stmts = vec![];

	let input_expr = p.parse_expr();
	p.expect(&token::Comma);

	let input_expr = quote_expr!(cx, {
		use std::str::Str;

		rt::Ok(($input_expr).as_slice())
	});

	let (arms, fallback) = parse_scan_body(cx, &mut p, sp);

	debug!("expand_scan - making scan expression");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback, NormalScan);
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
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback, NormalScan);
	debug!("expand_scanln - scan_expr: {}", scan_expr);
	MacExpr::new(scan_expr)
}

pub fn expand_scanln_from(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	debug!("expand_scanln_from(cx, sp, tts)");
	let mut p = cx.new_parser_from_tts(tts);

	let input_arg = p.parse_expr();
	p.expect(&token::Comma);

	let setup_stmts = vec![
		quote_stmt!(cx, let line = rt::io::read_line($input_arg);),
		quote_stmt!(cx, let line_str = match &line {
			&Err(ref err) => Err(rt::ScanIoError(err.clone())),
			&Ok(ref line) => Ok(line.as_slice().trim_right_chars('\n').trim_right_chars('\r'))
		};),
	];

	let input_expr = quote_expr!(cx, line_str);

	let (arms, fallback) = parse_scan_body(cx, &mut p, sp);

	debug!("expand_scanln - making scan expression...");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback, NormalScan);
	debug!("expand_scanln - scan_expr: {}", scan_expr);
	MacExpr::new(scan_expr)
}

pub fn expand_scanner(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	debug!("expand_scanner(cx, sp, tts)");
	let mut p = cx.new_parser_from_tts(tts);

	let scan_ty = p.parse_ty();
	p.expect(&token::Comma);

	let setup_stmts = vec![];

	let input_expr = quote_expr!(cx, cursor);

	let (arms, fallback) = parse_scan_body(cx, &mut p, sp);

	debug!("expand_scanner - making scan expression...");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback, ScannerScan);
	debug!("expand_scanner - scan_expr: {}", scan_expr);

	debug!("expand_scanner - making impl...");
	let scanner_impl = quote_item!(cx,
		impl<'a> ::scan_util::Scanner<'a> for $scan_ty {
			fn scan<Cur: ::scan_util::ScanCursor<'a>>(cursor: &Cur) -> Result<($scan_ty, Cur), ::scan_util::ScanError> {
				$scan_expr
			}
		}
	).unwrap();
	debug!("expand_scanner - scanner_impl: {}", scanner_impl);

	MacItems::new(vec![scanner_impl].into_iter())
}

fn parse_scan_body(cx: &mut ExtCtxt, p: &mut Parser, sp: codemap::Span) -> (Vec<(ScanArm, P<ast::Expr>)>, Option<(ScanArm, P<ast::Expr>)>) {
	let mut arms = vec![];
	let mut fallback = None;

	while p.token != token::Eof && fallback.is_none() {
		debug!("parse_scan_body - parsing scan arm");
		match parse_scan_arm(cx, p) {
			arm @ (FallbackArm(_), _) => fallback = Some(arm),
			arm @ (PatternArm(..), _) => arms.push(arm),
		}
	}

	if fallback.is_some() && p.token != token::Eof {
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

fn make_scan_expr(cx: &mut ExtCtxt, setup_stmts: Vec<P<ast::Stmt>>, input_expr: P<ast::Expr>, arms: Vec<(ScanArm, P<ast::Expr>)>, fallback_arm: Option<(ScanArm, P<ast::Expr>)>, scan_kind: ScanKind) -> P<ast::Expr> {
	debug!("make_scan_expr(...)");

	debug!("make_scan_expr - generating module setup statement...");
	let mod_setup_stmt = quote_stmt!(cx,
		mod rt {
			extern crate scan_util;
			pub use std::option::Option::{None, Some};
			pub use std::result::Result::{self, Err, Ok};
			pub use std::vec::Vec;
			pub use self::scan_util::{
				Cursor, ScanCursor,
				io,
				tokenizer,
				whitespace,
				compare_strs,
				ScanError, OtherScanError, ScanIoError,
				Scanner,
			};
		});

	let mut scan_arm_stmts = vec![];

	debug!("make_scan_expr - generating scan arms...");
	for (i,arm) in arms.into_iter().enumerate() {
		scan_arm_stmts.push(gen_arm_stmt(cx, arm, i == 0, scan_kind))
	}

	match fallback_arm {
		None => (),
		Some(arm) => {
			debug!("make_scan_expr - generating scan fallback arm...");
			let is_first = scan_arm_stmts.len() == 0;
			scan_arm_stmts.push(gen_arm_stmt(cx, arm, is_first, scan_kind))
		}
	}

	let match_expr = match scan_kind {
		NormalScan => {
			debug!("make_scan_expr - generating error arm");
			let err_arm = cx.arm(DUMMY_SP,
				vec![quote_pat!(cx, rt::Err(err))],
				quote_expr!(cx, rt::Err(err)),
			);

			debug!("make_scan_expr - generating ok arm");
			let ok_arm = cx.arm(
				DUMMY_SP,
				vec![quote_pat!(cx, rt::Ok(_input))],
				/*quote_expr!(cx, {
					let cur = rt::Cursor::new(
						_input,
						rt::tokenizer::WordsAndInts,
						rt::whitespace::Ignore,
						rt::compare_strs::CaseInsensitive);
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
			cx.expr_match(DUMMY_SP,
				input_expr,
				vec![err_arm, ok_arm],
			)
		},
		ScannerScan => {
			// A scanner doesn't use all the match setup; we already have a cursor!
			debug!("make_scan_expr - building passthru expr");
			/*quote_expr!(cx, {
				let cur = $input_expr;
				let mut result: rt::Result<_, rt::ScanError>;

				$scan_arm_stmts

				result
			})*/
			cx.expr_block(
				cx.block(DUMMY_SP,
					vec![
						cx.stmt_let(DUMMY_SP,
							/*mutabl:*/false, cx.ident_of("cur"),
							input_expr
						),
						quote_stmt!(cx, let mut result: rt::Result<_, rt::ScanError>;),
					] + &*scan_arm_stmts,
					Some(quote_expr!(cx, result))
				)
			)
		},
	};

	debug!("make_scan_expr - building block expr");
	/*let expr = quote_expr!(cx, {
		use scan_util::Scanner;
		use scan_util::ScanCursor;
		$mod_setup_stmt
		$setup_stmts
		$match_expr
	});*/
	let allow_unused_import = cx.attribute(DUMMY_SP,
		cx.meta_list(DUMMY_SP,
			token::intern_and_get_ident("allow"),
			vec![
				cx.meta_word(DUMMY_SP, token::intern_and_get_ident("unused_imports")),
			]
		)
	);
	let expr = cx.expr_block(
		cx.block_all(DUMMY_SP,
			/*view_items:*/vec![
				// This kinda sucks, but I can't find a way around this.  This requires the user to add an explicit `extern crate scan_util;` to their root module.
				set_view_item_attrs(
					vec![
						allow_unused_import.clone(),
					],
					cx.view_use_simple(DUMMY_SP,
						ast::Inherited,
						cx.path/*_global*/(DUMMY_SP,
							vec![
								cx.ident_of("scan_util"),
								cx.ident_of("Scanner"),
							]
						)
					)
				),
				set_view_item_attrs(
					vec![
						allow_unused_import.clone(),
					],
					cx.view_use_simple(DUMMY_SP,
						ast::Inherited,
						cx.path/*_global*/(DUMMY_SP,
							vec![
								cx.ident_of("scan_util"),
								cx.ident_of("ScanCursor"),
							]
						)
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

fn gen_arm_stmt(cx: &mut ExtCtxt, arm: (ScanArm, P<ast::Expr>), is_first: bool, scan_kind: ScanKind) -> P<ast::Stmt> {
	debug!("gen_arm_stmt(cx, {}, {}, {})", arm, is_first, scan_kind);
	let (arm_pat, arm_expr) = arm;

	let arm_expr = match scan_kind {
		NormalScan => {
			// We need to wrap arm_expr in a block and a partially constrained Ok.  If we don't wrap it in a block, `break` won't work---the compiler will complain about an unconstrained type.
			/*let arm_expr = quote_expr!(cx, rt::Ok::<_, rt::ScanError>({ $arm_expr }));*/
			cx.expr_call(DUMMY_SP,
				quote_expr!(cx, rt::Ok::<_, rt::ScanError>),
				vec![
					cx.expr_block(cx.block_expr(arm_expr)),
				]
			)
		},
		ScannerScan => {
			// We need to do more or less the same as with NormalScan *except* that we also want to bundle the cursor in with the result.
			/*let arm_expr = quote_expr!(cx, rt::Ok::<_, rt::ScanError>(({ $arm_expr }, cur)));*/
			cx.expr_call(DUMMY_SP,
				quote_expr!(cx, rt::Ok::<_, rt::ScanError>),
				vec![
					cx.expr_tuple(DUMMY_SP,
						vec![
							cx.expr_block(cx.block_expr(arm_expr)),
							quote_expr!(cx, cur),
						]
					),
				]
			)
		}
	};

	let (arm_trace_msg, cur_stmt, arm_expr) = match arm_pat {
		PatternArm(pattern, tail, attrs) => {
			let arm_trace_msg = if attrs.trace {
				Some(format!("matching against pattern {}", pattern.to_source()))
			} else {
				None
			};
			let cur_stmt = match scan_kind {
				NormalScan => {
					let tok = cx.expr_path(cx.path(DUMMY_SP,
						attrs.inp_tok.as_slice().split_str("::").map(|s| cx.ident_of(s)).collect()
					));
					let sp = cx.expr_path(cx.path(DUMMY_SP,
						attrs.inp_sp.as_slice().split_str("::").map(|s| cx.ident_of(s)).collect()
					));
					let cs = cx.expr_path(cx.path(DUMMY_SP,
						attrs.inp_cs.as_slice().split_str("::").map(|s| cx.ident_of(s)).collect()
					));
					let cur_stmt = quote_stmt!(cx,
						let cur = rt::Cursor::new(_input, $tok, $sp, $cs);
					);
					Some(cur_stmt)
				},
				ScannerScan => {
					Some(quote_stmt!(cx, let cur = cur.clone();))
				},
			};
			let and_then = match tail {
				None => {
					match scan_kind {
						NormalScan => {
							/*quote_expr!(cx, {
								match cur.expect_eof() {
									rt::Err(err) => rt::Err(err),
									rt::Ok(()) => $arm_expr
								}
							})*/
							cx.expr_match(DUMMY_SP,
								quote_expr!(cx, cur.expect_eof()),
								vec![
									cx.arm(DUMMY_SP,
										vec![quote_pat!(cx, rt::Err(err))],
										quote_expr!(cx, rt::Err(err))
									),
									cx.arm(DUMMY_SP,
										vec![quote_pat!(cx, rt::Ok(()))],
										arm_expr
									)
								]
							)
						},
						ScannerScan => {
							/*quote_expr!(cx, {
								$arm_expr
							})*/
							arm_expr
						},
					}
				},
				Some(ident) => {
					match scan_kind {
						NormalScan => {
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
						},
						ScannerScan => {
							/*quote_expr!(cx, {
								let tail_str = cur.tail_str();
								let $ident = tail_str;
								let cur = cur.slice_from(tail_str.len());
								$arm_expr
							})*/
							cx.expr_block(
								cx.block(DUMMY_SP,
									vec![
										quote_stmt!(cx, let tail_str = cur.tail_str();),
										cx.stmt_let(
											ident.span, /*mutabl:*/false, ident.node,
											quote_expr!(cx, tail_str)
										),
										quote_stmt!(cx, let cur = cur.slice_from(tail_str.len());),
									],
									Some(arm_expr)
								)
							)
						},
					}
				}
			};
			(arm_trace_msg, cur_stmt, gen_ast_scan_expr(cx, &attrs, pattern, and_then))
		},
		FallbackArm(None) => {
			match scan_kind {
				NormalScan => {
					(None, None, arm_expr)
				},
				ScannerScan => {
					(None, None, cx.expr_tuple(DUMMY_SP,
						vec![
							arm_expr,
							quote_expr!(cx, cur)
						]
					))
				},
			}
		},
		FallbackArm(Some(ident)) => {
			match scan_kind {
				NormalScan => {
					/*quote_expr!(cx, {
						let $ident = _input;
						$arm_expr
					})*/
					(None, None, cx.expr_block(
						cx.block(DUMMY_SP,
							vec![
								cx.stmt_let(ident.span, /*mutbl:*/false, ident.node, quote_expr!(cx, _input)),
							],
							Some(arm_expr)
						)
					))
				},
				ScannerScan => {
					/*quote_expr!(cx, {
						let $ident = _input;
						let cur = cur.slice_from(_input.len())
						$arm_expr
					})*/
					(None, None, cx.expr_block(
						cx.block(DUMMY_SP,
							vec![
								cx.stmt_let(ident.span, /*mutbl:*/false, ident.node, quote_expr!(cx, _input)),
								quote_stmt!(cx, let cur = cur.slice_from(_input.len());),
							],
							Some(arm_expr)
						)
					))
				},
			}
		},
	};

	let arm_trace_stmt = arm_trace_msg.map(|msg| {
		let msg = msg.as_slice();
		quote_stmt!(cx, debug!("{}", $msg);)
	});

	// Merge cur_stmt and arm_expr into a block.
	let arm_expr = cx.expr_block(cx.block(DUMMY_SP,
		arm_trace_stmt.into_iter()
			.chain(cur_stmt.into_iter())
			.collect(),
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

fn gen_ast_scan_expr(cx: &mut ExtCtxt, attrs: &ScanArmAttrs, node: PatAst, and_then: P<ast::Expr>) -> P<ast::Expr> {
	use parse::{AstAlternates, AstSequence, AstText, AstRegex, AstOptional, AstCapture, AstSliceCapture, AstLookahead, AstRepetition, RepeatRange};
	debug!("gen_ast_scan_expr(cx, {}, {}, {})", attrs, node, and_then);

	let captures = enumerate_captures(&node);

	let node_str = node.to_source();

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

					// Every result starts with the cursor.  Note that we're cloning the cursor here to get around not being able to move outer variables captured by reference.
					let cur_iter = Some(quote_expr!(cx, cur.clone())).into_iter();

					// Turn it into a tuple expr, wrap in an Ok.
					let result_expr = cx.expr_ok(DUMMY_SP,
						cx.expr_tuple(DUMMY_SP, cur_iter.chain(alt_results).collect())
					);

					if attrs.trace {
						let msg = format!("alt branch {} matched", node.to_source());
						let msg = msg.as_slice();
						quote_expr!(cx, {
							debug!("{}", $msg);
							$result_expr
						})
					} else {
						result_expr
					}
				};

				let trace_msg = format!("trying alt branch {}", node.to_source());

				// Now generate the match expr.
				let alt_expr = gen_ast_scan_expr(cx, attrs, node, alt_and_then);

				if attrs.trace {
					let msg = trace_msg; //format!("trying alt branch {}", node);
					let msg = msg.as_slice();
					cx.expr_block(cx.block(DUMMY_SP,
						vec![
							quote_stmt!(cx, debug!("{}", $msg);),
						],
						Some(alt_expr)
					))
				} else {
					alt_expr
				}
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
			{
				let expr = cx.expr_match(DUMMY_SP,
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
				);
				expr.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
					|| quote_stmt!(cx, debug!("try {}", $node_str);))
			}
		},
		AstSequence(nodes) => {
			// We need to do this *backwards* because in order to generate the AST for a node, we need to have the and_then AST.
			nodes.into_iter().rev()
				.fold(and_then, |and_then, node| gen_ast_scan_expr(cx, attrs, node, and_then))
				.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
					|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
		AstText(s) => {
			use parse::{WordsAndInts, IdentsAndInts, SpaceDelimited, ExplicitTok};
			use parse::{Ignore, ExplicitNewline, ExplicitSp, ExplicitAny, ExactSp};
			use parse::{CaseInsensitive, AsciiCaseInsensitive, ExactCS};
			use scan_util::{Tokenizer, tokenizer, Whitespace, whitespace, CompareStrs, compare_strs};

			let tc = match attrs.pat_tok {
				WordsAndInts => box tokenizer::WordsAndInts as Box<Tokenizer>,
				IdentsAndInts => box tokenizer::IdentsAndInts as Box<Tokenizer>,
				SpaceDelimited => box tokenizer::SpaceDelimited as Box<Tokenizer>,
				ExplicitTok => box tokenizer::Explicit as Box<Tokenizer>,
			};
			let sp = match attrs.pat_sp {
				Ignore => box whitespace::Ignore as Box<Whitespace>,
				ExplicitNewline => box whitespace::ExplicitNewline as Box<Whitespace>,
				ExplicitSp => box whitespace::Explicit as Box<Whitespace>,
				ExplicitAny => box whitespace::ExplicitAny as Box<Whitespace>,
				ExactSp => box whitespace::Exact as Box<Whitespace>,
			};
			let cs = match attrs.pat_cs {
				CaseInsensitive => box compare_strs::CaseInsensitive as Box<CompareStrs>,
				AsciiCaseInsensitive => box compare_strs::AsciiCaseInsensitive as Box<CompareStrs>,
				ExactCS => box compare_strs::Exact as Box<CompareStrs>,
			};

			let mut and_then = and_then;

			for tok in text_to_tokens(s.as_slice(), &*tc, &*sp, &*cs).into_iter().rev() {
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
									/*expr:*/
									cx.expr_call(DUMMY_SP,
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
									).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
										|| quote_stmt!(cx, debug!("did not match {}", err);))
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
									/*expr:*/and_then.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
										|| quote_stmt!(cx, debug!("matched");))
								)
							]
						)
					)
				);
			}

			and_then.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
				|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
		AstRegex(re) => {
			let err_trace = if attrs.trace {
				Some(quote_stmt!(cx, debug!("did not match {}", err);))
			} else {
				None
			};
			/*quote_expr!(cx, {
				let cur = cur.pop_ws();
				match (regex!($re)).find(cur.tail_str()) {
					rt::Some((0, end)) => {
						let cur = cur.slice_from(end);
						$and_then
					},
					rt::Some(..) | rt::None => rt::Err(cur.expected(format!("a match for /{}/", $re).as_slice()))
				}
			})*/
			cx.expr_block(cx.block(DUMMY_SP,
				vec![
					quote_stmt!(cx, let cur = cur.pop_ws();),
				],
				Some(cx.expr_match(DUMMY_SP,
					quote_expr!(cx, (regex!($re)).find(cur.tail_str())),
					vec![
						cx.arm(DUMMY_SP,
							vec![
								quote_pat!(cx, rt::Some((0, end))),
							],
							cx.expr_block(cx.block(DUMMY_SP,
								vec![
									quote_stmt!(cx, let cur = cur.slice_from(end);),
								],
								Some(and_then)
							))
						),
						quote_arm!(cx, rt::Some(..) | rt::None => {
							let err = cur.expected(format!("a match for /{}/", $re).as_slice());
							$err_trace
							rt::Err(err)
						},),
					]
				))
			)).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
				|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
		AstOptional(box node) => {
			// Desugars into ($node|).
			let alts = vec![node, AstSequence(vec![])];
			gen_ast_scan_expr(cx, attrs, AstAlternates(alts), and_then)
		},
		AstCapture(ident, ty) => {
			/*quote_expr!(cx, {
				let cur_tmp = cur.pop_ws(); // Work around possible bug in rustc.
				match rt::Scanner::scan(&cur_tmp) {
					Err(err) => Err(err),
					Ok((val, cur)) => {
						${
							if ident != "_" {
								${let $ident: $ty = val;}
							}
						}

						$and_then
					}
				}
			})*/
			cx.expr_block(cx.block(DUMMY_SP,
				vec![
					quote_stmt!(cx, let cur_tmp = cur.pop_ws();),
				],
				Some(cx.expr_match(DUMMY_SP,
					quote_expr!(cx, rt::Scanner::scan(&cur_tmp)),
					vec![
						{
							let trace_stmt = if attrs.trace {
								Some(quote_stmt!(cx, debug!("did not match {}", err);))
							} else {
								None
							};
							quote_arm!(cx, rt::Err(err) => { $trace_stmt rt::Err(err) },)
						},
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
										match (ident.node.as_str() != "_", ty) {
											(_, None) => cx.stmt_let(ident.span,
												/*mutbl:*/false,
												ident.node,
												quote_expr!(cx, val)
											),
											(_, Some(ty)) => cx.stmt_let_typed(ident.span,
												/*mutbl:*/false,
												ident.node,
												ty,
												quote_expr!(cx, val)
											),
											//(false, _) => quote_stmt!(cx, let _ = ();)
										},
									],
									Some(and_then)
								)
							).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
								|| quote_stmt!(cx, debug!("matched");))
						)
					]
				))
			)).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
				|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
		AstSliceCapture(ident, box node) => {
			// This is a bit like a cross between a single-branch alternate and a repetition.  We have similar capture-return behaviour to the former, similar sub-pattern scanning to the latter.

			if ident.node.as_str() == "_" {
				// Completely ignore all this if the ident is `_`.
				gen_ast_scan_expr(cx, attrs, node, and_then)
			} else {
				// Get list of captures in the sub-pattern.
				let node_captures = enumerate_captures(&node);

				// We use this to pass back the scanned capture's cursor and sub-captures.
				/*let node_and_then = quote_expr!(cx, {
					(cur $(, $node_captures)*)
				});*/
				let node_and_then = cx.expr_ok(DUMMY_SP,
					cx.expr_tuple(DUMMY_SP,
						Some(quote_expr!(cx, cur)).into_iter()
							.chain(
								node_captures.iter().map(|(&ident, &(sp, _))| {
									cx.expr_ident(sp, ident)
								})
							)
							.collect()
					)
				);

				// Build the expression to scan the sub-pattern.
				let node_expr = gen_ast_scan_expr(cx, attrs, node, node_and_then);

				// Build the capture pattern.
				/*let capture_pattern = quote_pat!(cx,
					(cur $(, $node_captures)*)
				);*/
				let capture_pattern = cx.pat_tuple(DUMMY_SP,
					Some(quote_pat!(cx, cur)).into_iter()
						.chain(
							node_captures.keys().map(|&ident| {
								cx.pat_ident(DUMMY_SP, ident)
							})
						)
						.collect()
				);

				/*quote_expr!(cx {
					let start_cur = cur.pop_ws();
					match $node_expr {
						Err(err) => Err(err),
						Ok($capture_pattern) => {
							let $ident = start_cur.str_slice_to_cur(&cur);
							$and_then
						}
					}
				})*/
				cx.expr_block(
					cx.block(DUMMY_SP,
						vec![
							quote_stmt!(cx, let start_cur = cur.pop_ws();),
						],
						Some(cx.expr_match(DUMMY_SP,
							node_expr,
							vec![
								{
									let trace_stmt = attrs.trace.map(|| quote_stmt!(cx,
										debug!("did not match {}", err);));
									quote_arm!(cx, Err(err) => { $trace_stmt Err(err) },)
								},
								cx.arm(DUMMY_SP,
									vec![
										cx.pat_ok(DUMMY_SP, capture_pattern),
									],
									cx.expr_block(cx.block(DUMMY_SP,
										vec![
											/*quote_stmt!(cx, let $ident = start_cur.str_slice_to_cur(&cur);),*/
											cx.stmt_let(ident.span,
												/*mutbl:*/false, ident.node,
												quote_expr!(cx, start_cur.str_slice_to_cur(&cur))
											),
										],
										Some(and_then.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
											|| quote_stmt!(cx, debug!("matched");)))
									))
								)
							]
						))
					)
				).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
					|| quote_stmt!(cx, debug!("try {}", $node_str);))
			}
		},
		AstLookahead(box node) => {
			let node_captures = enumerate_captures(&node);
			if node_captures.len() > 0 {
				cx.span_fatal(DUMMY_SP, "negative lookahead assertions cannot have captures");
			}

			// With a negative lookahead, we don't care about what we match.  We only capture `cur` here to shut the compiler up about unused variables.
			let node_and_then = quote_expr!(cx, rt::Ok(cur));
			let node_expr = gen_ast_scan_expr(cx, attrs, node, node_and_then);

			/*quote_expr!(cx, {
				match $node_expr {
					rt::Err(_) => $and_then,
					rt::Ok(_) => rt::Err(cur.expected("negative lookahead to fail"))
				}
			})*/
			cx.expr_match(DUMMY_SP,
				node_expr,
				vec![
					cx.arm(DUMMY_SP,
						vec![
							cx.pat_err(DUMMY_SP, quote_pat!(cx, _)),
						],
						and_then.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
							|| quote_stmt!(cx, debug!("negative lookahead passed");))
					),
					{
						let trace_stmt = attrs.trace.map(|| quote_stmt!(cx, debug!("negative lookahead failed");));
						quote_arm!(cx,
							rt::Ok(_) => { $trace_stmt rt::Err(cur.expected("negative lookahead to fail")) },
						)
					},
				]
			).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
				|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
		AstRepetition { node, sep, range } => {
			let node_captures = enumerate_captures(&*node);
			let sep_captures = sep.as_ref()
				.map(|sep| enumerate_captures(&**sep))
				.unwrap_or(HashMap::new());

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
				#[allow(unused_comparisons)]
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
				] + &*captures.iter().map(|(&ident, &(sp, _))| {
					let ty = cx.ty_infer(sp);
					cx.stmt_let_typed(
						sp,
						/*mutbl:*/true,
						cx.ident_of(format!("vec_{}",ident.as_str()).as_slice()),
						quote_ty!(cx, rt::Vec<$ty>),
						quote_expr!(cx, rt::Vec::new())
					)
				}).collect::<Vec<_>>() + &*vec![
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
																		] + &*sep_captures.iter().map(|(&ident, &(sp, _))| {
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
													] + &*node_captures.iter().map(|(&ident, &(sp, _))| {
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
							#[allow(unused_comparisons)]
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
					{
						let trace_stmt = attrs.trace.map(|| quote_stmt!(cx, debug!("did not match {}", err);));
						quote_arm!(cx, Err(err) => { $trace_stmt Err(err) },)
					},
					cx.arm(DUMMY_SP,
						vec![
							cx.pat_ok(DUMMY_SP, rep_pat),
						],
						and_then.maybe_prefix_stmt(attrs.trace, DUMMY_SP,
							|| quote_stmt!(cx, debug!("matched");))
					)
				]
			).maybe_prefix_stmt(attrs.trace, DUMMY_SP,
				|| quote_stmt!(cx, debug!("try {}", $node_str);))
		},
	}
}

// Annoyingly, Span doesn't implement Ord, hence why we have to split this up.
type CaptureMap = HashMap<ast::Ident, (codemap::Span, Option<P<ast::Ty>>)>;

fn enumerate_captures(node: &PatAst) -> CaptureMap {
	use parse::{AstAlternates, AstSequence, AstText, AstRegex, AstOptional, AstCapture, AstSliceCapture, AstLookahead, AstRepetition};
	debug!("enumerate_captures(&{})", node);

	fn merge(mut lhs: CaptureMap, rhs: CaptureMap) -> CaptureMap {
		// TODO: Ensure types are the same.
		lhs.extend(rhs.into_iter());
		lhs
	}

	match node {
		&AstAlternates(ref nodes) | &AstSequence(ref nodes) => {
			nodes.iter().map(enumerate_captures).fold(HashMap::new(), |a,b| merge(a,b))
		},
		&AstText(..) | &AstRegex(..) => {
			HashMap::new()
		},
		&AstOptional(ref node) => {
			enumerate_captures(&**node)
		},
		&AstCapture(ident, ref ty) => {
			let mut set = HashMap::new();
			// Ignore "_" as a capture.
			if ident.node.as_str() != "_" {
				set.insert(ident.node, (ident.span, ty.clone()));
			}
			set
		},
		&AstSliceCapture(ident, ref node) => {
			let mut captures = enumerate_captures(&**node);
			// Ignore "_" as a capture.
			if ident.node.as_str() != "_" {
				captures.insert(ident.node, (ident.span, None));
			}
			captures
		},
		&AstLookahead(ref node) => {
			enumerate_captures(&**node)
		}
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

fn text_to_tokens<'a>(s: &'a str, tc: &Tokenizer, sp: &Whitespace, _: &CompareStrs) -> Vec<&'a str> {
	let mut toks = vec![];
	let mut s = s;

	while s.len() > 0 {
		s = s.slice_from(sp.strip_len(s));

		match sp.token_len(s) {
			Some((end, tok)) => {
				toks.push(tok);
				s = s.slice_from(end);
				continue;
			}
			None => (),
		}

		s = s.slice_from(sp.strip_len(s));
		match tc.token_len(s) {
			Some(end) => {
				toks.push(s.slice_to(end));
				s = s.slice_from(end);
			},
			None => {
				if s.len() > 0 {
					let cr = s.char_range_at(0);
					toks.push(s.slice_to(cr.next));
					s = s.slice_from(cr.next);
				}
			}
		}
	}

	toks
}

fn str_to_expr<S: Str>(cx: &mut ExtCtxt, s: S) -> P<ast::Expr> {
	cx.expr_str(codemap::DUMMY_SP, token::intern_and_get_ident(s.as_slice()))
}

fn set_view_item_attrs(attrs: Vec<ast::Attribute>, mut vi: ast::ViewItem) -> ast::ViewItem {
	assert_eq!(vi.attrs.len(), 0);
	vi.attrs = attrs;
	vi
}

trait FakeToTokens {
	fn to_tokens(&self, cx: &ExtCtxt) -> Vec<ast::TokenTree>;
}

impl FakeToTokens for String {
	fn to_tokens(&self, cx: &ExtCtxt) -> Vec<ast::TokenTree> {
		self.as_slice().to_tokens(cx)
	}
}

trait MaybePrefixStatement {
	fn maybe_prefix_stmt(self, do_prefix: bool, sp: codemap::Span, stmt_f: || -> P<ast::Stmt>) -> P<ast::Expr>;
}

impl MaybePrefixStatement for P<ast::Expr> {
	fn maybe_prefix_stmt(self, do_prefix: bool, sp: codemap::Span, stmt_f: || -> P<ast::Stmt>) -> P<ast::Expr> {
		if do_prefix {
			P(ast::Expr {
				id: ast::DUMMY_NODE_ID,
				node: ast::ExprBlock(P(ast::Block {
					view_items: vec![],
					stmts: vec![stmt_f()],
					expr: Some(self),
					id: ast::DUMMY_NODE_ID,
					rules: ast::DefaultBlock,
					span: sp,
				})),
				span: sp,
			})
		} else {
			self
		}
	}
}

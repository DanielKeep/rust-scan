use syntax::ast;
use syntax::codemap;
use syntax::codemap::DUMMY_SP;
use syntax::ext::build::AstBuilder;
use syntax::ext::base::{ExtCtxt, MacExpr, MacResult};
use syntax::parse::token;
use syntax::ptr::P;

use parse::parse_scan_arm;
use parse::{ScanArm, FallbackArm, PatternArm};
use parse::PatAst;

use scan_util::{Tokenizer, Whitespace};

pub fn expand_scanln(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
	let mut p = cx.new_parser_from_tts(tts);

	let setup_stmts = vec![
		quote_stmt!(cx, let line = ::std::io::stdin().read_line();),
		quote_stmt!(cx, let line_str = match &line {
			&Err(ref err) => Err(rt::ScanIoError(err.clone())),
			&Ok(ref line) => Ok(line.as_slice().trim_right_chars('\n').trim_right_chars('\r'))
		};),
	];

	let input_expr = quote_expr!(cx, line_str);

	let mut arms = vec![];
	let mut fallback = None;

	while p.token != token::EOF && fallback.is_none() {
		match parse_scan_arm(cx, &mut p) {
			arm @ (FallbackArm(_), _) => fallback = Some(arm),
			arm @ (PatternArm(_, _), _) => arms.push(arm),
		}
	}

	if fallback.is_some() && p.token != token::EOF {
		let sp = p.span;
		let tok_str = p.this_token_to_string();
		p.span_note(sp, format!("there shouldn't be anything after the fallback arm, found `{}`", tok_str).as_slice());
		p.unexpected();
	}

	if arms.len() == 0 && fallback.is_none() {
		p.span_fatal(sp, "expected at least one scan arm");
	}

	debug!("expand_scanln - making scan expression...");
	let scan_expr = make_scan_expr(cx, setup_stmts, input_expr, arms, fallback);
	debug!("expand_scanln - scan_expr: {}", scan_expr);
	MacExpr::new(scan_expr)
}

fn make_scan_expr(cx: &mut ExtCtxt, setup_stmts: Vec<P<ast::Stmt>>, input_expr: P<ast::Expr>, arms: Vec<(ScanArm, P<ast::Expr>)>, fallback_arm: Option<(ScanArm, P<ast::Expr>)>) -> P<ast::Expr> {
	debug!("make_scan_expr(...)");

	debug!("make_scan_expr - generating module setup statement...");
	let mod_setup_stmt = quote_stmt!(cx,
		mod rt {
			extern crate scan_util;
			pub use std::result::{Result, Err, Ok};
			pub use self::scan_util::{
				Cursor,
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
							let cur = rt::Cursor::new(
								input,
								rt::tokenizer::WordsAndInts,
								rt::whitespace::Ignore);
						),
						quote_stmt!(cx,
							//let mut result = rt::Err(rt::NothingMatched);
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

	let arm_expr = match arm_pat {
		PatternArm(pattern, tail) => {
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
							None => Ok({
								$arm_expr
							})
						}
					})*/
					cx.expr_match(DUMMY_SP,
						quote_expr!(cx, cur.pop_token()),
						vec![
							cx.arm(DUMMY_SP,
								vec![quote_pat!(cx, Some((tok, cur)))],
								quote_expr!(cx,
									rt::Err(
										rt::OtherScanError(
											format!("expected end of input, got `{}`", tok),
											cur.consumed()
										)
									)
								)
							),
							cx.arm(DUMMY_SP,
								vec![quote_pat!(cx, None)],
								cx.expr_call(DUMMY_SP,
									quote_expr!(cx, rt::Ok),
									vec![
										arm_expr,
									]
								)
							)
						]
					)
				},
				Some(ident) => {
					/*quote_expr!(cx, {
						let $ident = cur.tail_str();
						rt::Ok({
							$arm_expr
						})
					})*/
					cx.expr_block(
						cx.block(DUMMY_SP,
							vec![
								cx.stmt_let(
									ident.span, /*mutbl:*/false, ident.node,
									quote_expr!(cx, cur.tail_str())
								),
							],
							Some(cx.expr_call(DUMMY_SP,
								quote_expr!(cx, rt::Ok::<_, rt::ScanError>),
								vec![
									arm_expr,
								]
							))
						)
					)
				}
			};
			gen_ast_scan_expr(cx, pattern, and_then)
		},
		FallbackArm(None) => {
			/*quote_expr!(cx, Ok($arm_expr))*/
			cx.expr_call(DUMMY_SP,
				quote_expr!(cx, rt::Ok),
				vec![arm_expr]
			)
		},
		FallbackArm(Some(ident)) => {
			/*quote_expr!(cx, {
				let $ident = cur.tail_str();
				rt::Ok($arm_expr)
			})*/
			cx.expr_block(
				cx.block(DUMMY_SP,
					vec![
						quote_stmt!(cx, let $ident = cur.tail_str();),
					],
					Some(cx.expr_call(DUMMY_SP,
						quote_expr!(cx, rt::Ok::<_, rt::ScanError>),
						vec![arm_expr]
					))
				)
			)
		},
	};

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

fn gen_ast_scan_expr(cx: &mut ExtCtxt, node: PatAst, and_then: P<ast::Expr>) -> P<ast::Expr> {
	use parse::{AstAlternates, AstSequence, AstText, AstOptional, AstCapture, AstRepetition};
	debug!("gen_ast_scan_expr(cx, {}, {})", node, and_then);
	match node {
		AstAlternates(_) => {
			fail!("NYI - alternates expr gen")
		},
		AstSequence(nodes) => {
			// We need to do this *backwards* because in order to generate the AST for a node, we need to have the and_then AST.
			nodes.into_iter().rev()
				.fold(and_then, |and_then, node| gen_ast_scan_expr(cx, node, and_then))
		},
		AstText(s) => {
			// TODO: allow these to be overridden.
			let tc = ::scan_util::tokenizer::WordsAndInts;
			let sp = ::scan_util::whitespace::Ignore;

			let mut and_then = and_then;

			for tok in text_to_tokens(s.as_slice(), &tc, &sp).into_iter().rev() {
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
		AstOptional(_) => {
			fail!("NYI - optional expr gen")
		},
		AstCapture(ident, ty) => {
			/*quote_expr!(cx, {
				match rt::Scanner::scan(&cur.pop_ws()) {
					Err(err) => Err(err),
					Ok((val, cur)) => {
						let val: $ty = val;
						let $ident = val.scanned_value();

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
										let val: $ty = val;
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
									/*quote_stmt!(cx,
										let $ident = val.scanned_value();
									),*/
									cx.stmt_let(ident.span,
										/*mutbl:*/false,
										ident.node,
										quote_expr!(cx, val.scanned_value())
									),
								],
								Some(and_then)
							)
						)
					)
				]
			)
		},
		AstRepetition { node: _, sep: _, range: _ } => {
			fail!("NYI - repetition expr gen")
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

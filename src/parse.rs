/*

# Pattern Grammar

	<pattern> := <attributes> <alternates>

	<attributes> := <attribute> <attributes>?

	<alternates> := <sequence> ("|" <alternates>)?

	<sequence> := <sequence-atom> <sequence-atom>?

	<sequence-atom> := <maybe-text> | <maybe-capture> | <maybe-group> | <repetition>

	<maybe-text> := <text> "?"?

	<text> := <string_literal> | <raw_string_literal>

	<maybe-capture> := <capture> "?"?

	<capture> := <identifier> <constraint>?

	<constraint> := ":" <type>

	<maybe-group> := <group> "?"?

	<group> := "(" <alternates> ")"

	<repetition> := "[" <alternates> "]" <separator>? <repeat-range>

	<separator> := "," | "." | ";" | ":" | <text> | <group>

	<repeat-range> := "?" | "*" | "+" | <numeric-range>

	<numeric-range> := "{" "," <uint> "}" | "{" <uint> ("," <uint>)? "}"

# Tokenisers

These can be specified on a pattern using the `#[tokens="..."]` syntax.

Note that all tokenisers have an implicit fallback wherein if no token is generated, the next character is turned into a token.  For example, the string `"How 'ya doin'?"`, assuming `#[tokenize="words_and_numbers"]` and `#[space="ignore"]` produces the tokens `"How"`, `"'"`, `"ya"`, `"doin"`, `"'"`, `"?"`.

* `words_and_ints` (default) - each token is one or more characters in the L? category
* `idents_and_ints` - each token is either one or more characters in the Nd category, or a single XID_Start character followed by zero or more XID_Continue characters.
* `non_space` - each token is one or more contiguous characters which do not have the WSpace property.
* `explicit` - each quoted string literal is turned into a single token.

# Whitespace Policies

These can be specified on a pattern using the `#[space="..."]` syntax.

These are used to determine how whitespace is treated, both when tokenising text in the pattern *and* when matching against input at runtime.

* `ignore` (default) - whitespace between tokens is ignored.
* `explicit_newline` - end of line sequences are turned into an explicit token; all other whitespace is ignored.
* `explicit` - runs of whitespace and end of line sequences are each turned into an explicit token.
* `explicit_any` - runs of whitespace and/or end of line sequences are all turned into a single explicit token.
* `exact` - all whitespace characters are turned into literal tokens.

# Case Rules

These can be specified on a pattern using the `#[case="..."]` syntax.

* `ignore` (default) - use case-insensitive matches.
* `exact` - use case-sensitive matches.

 */
use syntax::ast;
use syntax::codemap::{Spanned, respan};
use syntax::ext::base::ExtCtxt;
use syntax::parse::attr::ParserAttr;
use syntax::parse::parser;
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::ptr::P;

pub use self::scan_pattern::{PatAst, AstAlternates, AstSequence, AstText, AstOptional, AstCapture, AstRepetition};
pub use self::scan_pattern::RepeatRange;

#[deriving(Show)]
pub enum ScanArm {
	FallbackArm(Option<Spanned<ast::Ident>>),
	PatternArm(PatAst, Option<Spanned<ast::Ident>>),
}

pub fn parse_scan_arm(cx: &mut ExtCtxt, p: &mut Parser) -> (ScanArm, P<ast::Expr>) {
	let arm_attrs = p.parse_outer_attributes();
	assert!(arm_attrs.len() == 0, "NYI");
	let arm_pat = parse_scan_pattern(cx, p);
	let arm_expr = parse_arm_expr(cx, p);
	(arm_pat, arm_expr)
}

fn parse_scan_pattern(cx: &mut ExtCtxt, p: &mut Parser) -> ScanArm {
	fn parse_fallback_ident(_: &mut ExtCtxt, p: &mut Parser) -> Option<Spanned<ast::Ident>> {
		if p.eat(&token::UNDERSCORE) {
			None
		} else {
			Some(respan(p.span, p.parse_ident()))
		}
	}

	// This *might* be a fallback pattern.
	if p.token == token::DOTDOT {
		p.bump();
		let ident = parse_fallback_ident(cx, p);
		p.expect(&token::FAT_ARROW);
		return FallbackArm(ident)
	}

	let pat_ast = scan_pattern::parse_pattern(cx, p);

	p.expect_one_of(&[], &[token::COMMA, token::FAT_ARROW]);
	match p.bump_and_get() {
		token::COMMA => {
			// Has fallback.
			p.expect(&token::DOTDOT);
			let ident = parse_fallback_ident(cx, p);
			p.expect(&token::FAT_ARROW);
			PatternArm(pat_ast, ident)
		},
		token::FAT_ARROW => {
			// No fallback.
			PatternArm(pat_ast, None)
		},
		_ => fail!()
	}
}

fn parse_arm_expr(_: &mut ExtCtxt, p: &mut Parser) -> P<ast::Expr> {
	// Nicked from libsyntax/parse/parser.rs, fn parse_arm.
	let arm_expr = p.parse_expr_res(parser::RestrictionStmtExpr);

	{
		let require_comma =
			!::syntax::parse::classify::expr_is_simple_block(&*arm_expr)
			&& p.token != token::RBRACE;

		if require_comma {
			p.commit_expr(&*arm_expr, &[token::COMMA], &[token::RBRACE, token::EOF]);
		} else {
			p.eat(&token::COMMA);
		}
	}

	arm_expr
}

mod scan_pattern {
	use syntax::ast;
	use syntax::codemap::{Spanned, respan};
	use syntax::parse;
	use syntax::parse::parser::Parser;
	use syntax::parse::token;
	use syntax::ptr::P;
	use syntax::ext::base::ExtCtxt;

	#[deriving(Show)]
	pub enum PatAst {
		AstAlternates(Vec<PatAst>),
		AstSequence(Vec<PatAst>),
		AstText(String),
		AstOptional(Box<PatAst>),
		AstCapture(Spanned<ast::Ident>, Option<P<ast::Ty>>),
		AstRepetition {
			pub node: Box<PatAst>,
			pub sep: Option<Box<PatAst>>,
			pub range: RepeatRange
		},
	}

	#[deriving(Show)]
	pub struct RepeatRange(uint, Option<uint>);

	/*
		<pattern> := <attributes> <alternates>

	Note: the attributes are parsed at a higher level so that we can have them on fallback arms, too.
	*/
	pub fn parse_pattern(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		parse_alternates(cx, p)
	}

	// <alternates> := <sequence> ("|" <alternates>)?
	fn parse_alternates(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		let mut alts = vec![parse_sequence(cx, p)];
		while p.eat(&token::BINOP(token::OR)) {
			alts.push(parse_sequence(cx, p));
		}
		if alts.len() == 1 {
			alts.pop().unwrap()
		} else {
			AstAlternates(alts)
		}
	}

	// <sequence> := <sequence-atom>*
	fn parse_sequence(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		let mut nodes = vec![];
		loop {
			match try_parse_sequence_atom(cx, p) {
				Some(node) => nodes.push(node),
				None => break,
			}
		}
		AstSequence(nodes)
	}

	// <sequence-atom> := <maybe-text> | <maybe-capture> | <maybe-group> | <repetition>
	fn try_parse_sequence_atom(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		try_parse_maybe_text(cx, p)
			.or_else(|| try_parse_maybe_capture(cx, p))
			.or_else(|| try_parse_maybe_group(cx, p))
			.or_else(|| try_parse_repetition(cx, p))
	}

	// <maybe-text> := <text> "?"?
	fn try_parse_maybe_text(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		try_parse_text(cx, p)
			.and_then(|node| {
				if p.eat(&token::QUESTION) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <text> := <string_literal> | <raw_string_literal>
	fn try_parse_text(_: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		match p.token {
			token::LIT_STR(ident) => {
				p.bump();
				Some(AstText(ident.as_str().into_string()))
			},
			token::LIT_STR_RAW(ident, _) => {
				p.bump();
				Some(AstText(ident.as_str().into_string()))
			},
			_ => None
		}
	}

	// <maybe-capture> := <capture> "?"?
	fn try_parse_maybe_capture(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		try_parse_capture(cx, p)
			.and_then(|node| {
				if p.eat(&token::QUESTION) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <capture> := <identifier> <constraint>?
	fn try_parse_capture(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		let ident = match p.token {
			token::IDENT(ident, _) => {
				let sp = p.span;
				p.bump();
				respan(sp, ident)
			},
			_ => return None
		};

		Some(AstCapture(ident, try_parse_constraint(cx, p)))
	}

	// <constraint> := ":" <type>
	fn try_parse_constraint(_: &mut ExtCtxt, p: &mut Parser) -> Option<P<ast::Ty>> {
		if !p.eat(&token::COLON) {
			return None
		}

		Some(p.parse_ty(/*plus_allowed:*/false))
	}

	// <maybe-group> := <group> "?"?
	fn try_parse_maybe_group(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		try_parse_group(cx, p)
			.and_then(|node| {
				if p.eat(&token::QUESTION) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <group> := "(" <alternates> ")"
	fn try_parse_group(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		if !p.eat(&token::LPAREN) {
			return None
		}

		let node = parse_alternates(cx, p);
		p.expect(&token::RPAREN);

		Some(node)
	}

	// <repetition> := "[" <alternates> "]" <separator>? <repeat-range>
	fn try_parse_repetition(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		if !p.eat(&token::LBRACKET) {
			return None
		}

		let node = parse_alternates(cx, p);
		p.expect(&token::RBRACKET);

		let sep = try_parse_separator(cx, p).map(|n| box n);
		let range = parse_repeat_range(cx, p);

		Some(AstRepetition { node: box node, sep: sep, range: range })
	}

	// <separator> := "," | "." | ";" | ":" | <text> | <group>
	fn try_parse_separator(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		match p.token {
			token::COMMA => {
				p.bump();
				Some(AstText(",".into_string()))
			},
			token::DOT => {
				p.bump();
				Some(AstText(".".into_string()))
			},
			token::SEMI => {
				p.bump();
				Some(AstText(";".into_string()))
			},
			token::COLON => {
				p.bump();
				Some(AstText(":".into_string()))
			},
			_ => {
				try_parse_text(cx, p)
					.or_else(|| try_parse_group(cx, p))
			}
		}
	}

	// <repeat-range> := "?" | "*" | "+" | <numeric-range>
	fn parse_repeat_range(cx: &mut ExtCtxt, p: &mut Parser) -> RepeatRange {
		match p.token {
			token::QUESTION => {
				p.bump();
				RepeatRange(0, Some(1))
			},
			token::BINOP(token::STAR) => {
				p.bump();
				RepeatRange(0, None)
			},
			token::BINOP(token::PLUS) => {
				p.bump();
				RepeatRange(1, None)
			},
			token::LBRACE => parse_numeric_range(cx, p),
			_ => {
				p.fatal("expected `?`, `*`, `+` or a numeric repeat range")
			}
		}
	}

	// <numeric-range> := "{" "," <uint> "}" | "{" <uint> ("," <uint>?)? "}"
	fn parse_numeric_range(cx: &mut ExtCtxt, p: &mut Parser) -> RepeatRange {
		p.expect(&token::LBRACE);
		let (min, max) = match p.token {
			token::COMMA => {
				// {,max} -> {0, max}
				p.bump();
				(0, Some(parse_uint(cx, p)))
			},
			token::LIT_INTEGER(_) => {
				let min = parse_uint(cx, p);
				let max = if p.eat(&token::COMMA) {
					match try_parse_uint(cx, p) {
						Some(i) => Some(i), // {min, max}
						None => None        // {min,} -> {min, infinity}
					}
				} else {
					Some(min)               // {min} -> {min, min}
				};
				(min, max)
			},
			_ => p.fatal("expected `,` or an integer")
		};
		p.expect(&token::RBRACE);

		RepeatRange(min, max)
	}

	fn try_parse_uint(cx: &mut ExtCtxt, p: &mut Parser) -> Option<uint> {
		match p.token {
			token::LIT_INTEGER(_) => Some(parse_uint(cx, p)),
			_ => None
		}
	}

	fn parse_uint(_: &mut ExtCtxt, p: &mut Parser) -> uint {
		let int_lit = match p.bump_and_get() {
			token::LIT_INTEGER(s) => {
				parse::integer_lit(s.as_str(), &p.sess.span_diagnostic, p.span)
			},
			_ => p.fatal("expected integer literal")
		};

		// AFAIK, we can't get a negative integer literal from parse::integer_lit.
		// There's really only one potential issue; unsigned 64-bit on a 32-bit platform.
		let val = match int_lit {
			ast::LitInt(v, ast::SignedIntLit(_, ast::Plus)) => v,
			ast::LitInt(v, ast::UnsignedIntLit(_)) => v,
			ast::LitInt(v, ast::UnsuffixedIntLit(ast::Plus)) => v,
			_ => p.fatal("expected non-negative integer literal")
		};

		if !(val < (::std::uint::MAX as u64)) {
			p.fatal(format!("integer `{}` is too large (max is `{}`)", val, ::std::uint::MAX).as_slice())
		}

		val as uint
	}
}

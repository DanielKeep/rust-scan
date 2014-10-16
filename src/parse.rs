/*

# Pattern Grammar

	<pattern> := <attributes> <alternates>

	<attributes> := <attribute> <attributes>?

	<alternates> := <sequence> ("|" <alternates>)?

	<sequence> := <sequence-atom> <sequence-atom>?

	<sequence-atom> := <maybe-capture> | <non-capture-atom>

	<non-capture-atom> := <maybe-text> | <maybe-regex> | <maybe-group> | <repetition>

	<maybe-text> := <text> "?"?

	<text> := <string_literal> | <raw_string_literal>

	<maybe-regex> := <regex> "?"?

	<regex> := "/" <text>

	<maybe-capture> := <capture> "?"?

	<capture> := <identifier> ( <slice-capture> | <constraint>? )

	<slice-capture> := "=" <non-capture-atom>

	<constraint> := ":" <type>

	<maybe-group> := <group> "?"?

	<group> := "(" <lookahead>? <alternates> ")"

	<lookahead> := "?" "!"

	<repetition> := "[" <alternates> "]" <separator>? <repeat-range>

	<separator> := "," | "." | ";" | ":" | <text> | <group>

	<repeat-range> := "?" | "*" | "+" | <numeric-range>

	<numeric-range> := "{" "," <uint> "}" | "{" <uint> ("," <uint>)? "}"

# Tokenisers

These can be specified on a pattern using the `#[tokenizer="..."]` syntax.

Note that all tokenisers have an implicit fallback wherein if no token is generated, the next character is turned into a token.  For example, the string `"How 'ya doin'?"`, assuming `#[tokenizer="WordsAndInts"]` and `#[space="Ignore"]` produces the tokens `"How"`, `"'"`, `"ya"`, `"doin"`, `"'"`, `"?"`.

* `WordsAndInts` (default) - each token is one or more characters in the L? category
* `IdentsAndInts` - each token is either one or more characters in the Nd category, or a single XID_Start character followed by zero or more XID_Continue characters.
* `SpaceDelimited` - each token is one or more contiguous characters which do not have the WSpace property.
* `Explicit` - each quoted string literal is turned into a single token.

If you wish to use a different tokenizer at runtime, you can specify it with `#[runtime_tok="..."]`.  Note that in addition to the above values, `runtime_tok` can use any path to a value which implements the `Tokenizer` trait.

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

pub use self::scan_pattern::{PatAst, AstAlternates, AstSequence, AstText, AstRegex, AstOptional, AstCapture, AstSliceCapture, AstLookahead, AstRepetition};
pub use self::scan_pattern::RepeatRange;

#[deriving(Show)]
pub enum ScanArm {
	FallbackArm(Option<Spanned<ast::Ident>>),
	PatternArm(PatAst, Option<Spanned<ast::Ident>>, ScanAttrs),
}

#[deriving(Show)]
pub struct ScanAttrs {
	pub pat_tok: ArmTokenizer,
	pub inp_tok: String,
}

#[deriving(Show)]
pub enum ArmTokenizer {
	WordsAndInts,
	IdentsAndInts,
	SpaceDelimited,
	ExplicitTok,
}

pub fn parse_scan_arm(cx: &mut ExtCtxt, p: &mut Parser) -> (ScanArm, P<ast::Expr>) {
	debug!("parse_scan_arm(cx, p @ {})", p.token);
	let arm_attrs = parse_scan_arm_attrs(cx, p);
	let arm_pat = parse_scan_pattern(cx, p, arm_attrs);
	let arm_expr = parse_arm_expr(cx, p);
	(arm_pat, arm_expr)
}

fn parse_scan_arm_attrs(cx: &mut ExtCtxt, p: &mut Parser) -> ScanAttrs {
	debug!("parse_scan_arm_attrs(cx, p @ {})", p.token);

	fn lit_str<'a>(lit: &'a ast::Lit) -> Option<&'a str> {
		match &lit.node {
			&ast::LitStr(ref is, _) => Some(is.get()),
			_ => None
		}
	}

	let mut pat_tok = None;
	let mut inp_tok = None;

	for attr in p.parse_outer_attributes().into_iter() {
		match attr.node.value.node {
			ast::MetaNameValue(ref name, ref value) if name.get() == "tokenizer" => {
				let value_span = value.span;
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value_span, "tokenizer must be a string"));

				let (tok_enum, tok_str) = match value {
					"WordsAndInts" => (WordsAndInts, "WordsAndInts"),
					"IdentsAndInts" => (IdentsAndInts, "IdentsAndInts"),
					"SpaceDelimited" => (SpaceDelimited, "SpaceDelimited"),
					"Explicit" => (ExplicitTok, "Explicit"),
					_ => cx.span_fatal(value_span, "unrecognised tokeniser")
				};

				pat_tok = Some(tok_enum);
				if inp_tok.is_none() {
					inp_tok = Some(format!("rt::tokenizer::{}", tok_str));
				}
			},
			ast::MetaNameValue(ref name, ref value) if name.get() == "runtime_tok" => {
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value.span, "runtime_tok must be a string"));
				inp_tok = Some(value.into_string());
			},
			_ => {
				cx.span_fatal(attr.span, "unrecognised attribute")
			}
		}
	}

	let pat_tok = pat_tok.unwrap_or(WordsAndInts);
	let inp_tok = inp_tok.unwrap_or("rt::tokenizer::WordsAndInts".into_string());

	ScanAttrs {
		pat_tok: pat_tok,
		inp_tok: inp_tok,
	}
}

fn parse_scan_pattern(cx: &mut ExtCtxt, p: &mut Parser, attrs: ScanAttrs) -> ScanArm {
	debug!("parse_scan_pattern(cx, p @ {}, {})", p.token, attrs);
	debug!("parse_scan_pattern(cx, p @ {})", p.token);
	fn parse_fallback_ident(_: &mut ExtCtxt, p: &mut Parser) -> Option<Spanned<ast::Ident>> {
		if p.eat(&token::UNDERSCORE) {
			None
		} else {
			Some(respan(p.span, p.parse_ident()))
		}
	}

	// This *might* be a fallback pattern.
	if p.token == token::UNDERSCORE {
		p.bump();
		p.expect(&token::FAT_ARROW);
		return FallbackArm(None)
	} else if p.token == token::DOTDOT {
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
			PatternArm(pat_ast, ident, attrs)
		},
		token::FAT_ARROW => {
			// No fallback.
			PatternArm(pat_ast, None, attrs)
		},
		_ => fail!()
	}
}

fn parse_arm_expr(_: &mut ExtCtxt, p: &mut Parser) -> P<ast::Expr> {
	debug!("parse_arm_expr(cx, p @ {})", p.token);
	// Nicked from libsyntax/parse/parser.rs, fn parse_arm.
	debug!("parse_arm_expr - parsing stmt expr");
	let arm_expr = p.parse_expr_res(parser::RESTRICTION_STMT_EXPR);

	debug!("parse_arm_expr - p @ {}", p.token);

	{
		let require_comma =
			!::syntax::parse::classify::expr_is_simple_block(&*arm_expr)
			&& p.token != token::RBRACE;

		if require_comma {
			debug!("parse_arm_expr - require comma");
			p.commit_expr(&*arm_expr, &[token::COMMA], &[token::RBRACE, token::EOF]);
		} else {
			debug!("parse_arm_expr - don't require comma");
			p.eat(&token::COMMA);
		}
	}

	debug!("parse_arm_expr - done");
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
		AstRegex(String),
		AstOptional(Box<PatAst>),
		AstCapture(Spanned<ast::Ident>, Option<P<ast::Ty>>),
		AstSliceCapture(Spanned<ast::Ident>, Box<PatAst>),
		AstLookahead(Box<PatAst>),
		AstRepetition {
			pub node: Box<PatAst>,
			pub sep: Option<Box<PatAst>>,
			pub range: RepeatRange
		},
	}

	#[deriving(Show)]
	pub struct RepeatRange(pub uint, pub Option<uint>);

	/*
		<pattern> := <attributes> <alternates>

	Note: the attributes are parsed at a higher level so that we can have them on fallback arms, too.
	*/
	pub fn parse_pattern(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		debug!("parse_pattern(cx, p @ {})", p.token);
		parse_alternates(cx, p)
	}

	// <alternates> := <sequence> ("|" <alternates>)?
	fn parse_alternates(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		debug!("parse_alternates(cx, p @ {})", p.token);
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
		debug!("parse_sequence(cx, p @ {})", p.token);
		let mut nodes = vec![];
		loop {
			match try_parse_sequence_atom(cx, p) {
				Some(node) => nodes.push(node),
				None => break,
			}
		}
		AstSequence(nodes)
	}

	// <sequence-atom> := <maybe-capture> | <non-capture-atom>
	fn try_parse_sequence_atom(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_sequence_atom(cx, p @ {})", p.token);
		try_parse_maybe_capture(cx, p)
			.or_else(|| try_parse_non_capture_atom(cx, p))
	}

	// <non-capture-atom> := <maybe-text> | <maybe-regex> | <maybe-group> | <repetition>
	fn parse_non_capture_atom(cx: &mut ExtCtxt, p: &mut Parser) -> PatAst {
		debug!("parse_non_capture_atom(cx, p @ {})", p.token);
		if let Some(ast) = try_parse_non_capture_atom(cx, p) {
			ast
		} else {
			cx.span_fatal(p.span, "expected text, group or repetition")
		}
	}

	fn try_parse_non_capture_atom(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_non_capture_atom(cx, p @ {})", p.token);
		try_parse_maybe_text(cx, p)
			.or_else(|| try_parse_maybe_regex(cx, p))
			.or_else(|| try_parse_maybe_group(cx, p))
			.or_else(|| try_parse_repetition(cx, p))
	}

	// <maybe-text> := <text> "?"?
	fn try_parse_maybe_text(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_maybe_text(cx, p @ {})", p.token);
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
	fn try_parse_text(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_text(cx, p @ {})", p.token);
		try_parse_str_lit(cx, p).map(|s| AstText(s))
	}

	fn try_parse_str_lit(_: &mut ExtCtxt, p: &mut Parser) -> Option<String> {
		match p.token {
			token::LIT_STR(ident) => {
				p.bump();
				Some(::syntax::parse::str_lit(ident.as_str()))
			},
			token::LIT_STR_RAW(ident, _) => {
				p.bump();
				Some(::syntax::parse::raw_str_lit(ident.as_str()))
			},
			_ => None
		}
	}

	// <maybe-regex> := <regex> "?"?
	fn try_parse_maybe_regex(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_maybe_regex(cx, p @ {})", p.token);
		try_parse_regex(cx, p)
			.and_then(|node| {
				if p.eat(&token::QUESTION) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <regex> := "/" <text>
	fn try_parse_regex(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_regex(cx, p @ {})", p.token);
		if p.eat(&token::BINOP(token::SLASH)) {
			match try_parse_str_lit(cx, p) {
				Some(s) => Some(AstRegex(s)),
				None => p.fatal("expected string literal")
			}
		} else {
			None
		}
	}

	// <maybe-capture> := <capture> "?"?
	fn try_parse_maybe_capture(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_maybe_capture(cx, p @ {})", p.token);
		try_parse_capture(cx, p)
			.and_then(|node| {
				if p.eat(&token::QUESTION) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <capture> := <identifier> ( <slice-capture> | <constraint>? )
	fn try_parse_capture(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_capture(cx, p @ {})", p.token);
		let ident = match p.token {
			token::IDENT(ident, _) => {
				let sp = p.span;
				p.bump();
				respan(sp, ident)
			},
			_ => return None
		};

		if let Some(sub_pattern) = try_parse_slice_capture(cx, p) {
			Some(AstSliceCapture(ident, box sub_pattern))
		} else {
			Some(AstCapture(ident, try_parse_constraint(cx, p)))
		}
	}

	// <slice-capture> := "=" <non-capture-atom>
	fn try_parse_slice_capture(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_slice_capture(cx, p @ {})", p.token);
		if p.eat(&token::EQ) {
			Some(parse_non_capture_atom(cx, p))
		} else {
			None
		}
	}

	// <constraint> := ":" <type>
	fn try_parse_constraint(_: &mut ExtCtxt, p: &mut Parser) -> Option<P<ast::Ty>> {
		debug!("try_parse_constraint(cx, p @ {})", p.token);
		if !p.eat(&token::COLON) {
			return None
		}

		Some(p.parse_ty(/*plus_allowed:*/false))
	}

	// <maybe-group> := <group> "?"?
	fn try_parse_maybe_group(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_maybe_group(cx, p @ {})", p.token);
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
		debug!("try_parse_group(cx, p @ {})", p.token);
		if !p.eat(&token::LPAREN) {
			return None
		}

		let lookahead = if p.eat(&token::QUESTION) {
			p.expect(&token::NOT);
			true
		} else {
			false
		};

		let node = parse_alternates(cx, p);
		p.expect(&token::RPAREN);

		if lookahead {
			Some(AstLookahead(box node))
		} else {
			Some(node)
		}
	}

	// <repetition> := "[" <alternates> "]" <separator>? <repeat-range>
	fn try_parse_repetition(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_repetition(cx, p @ {})", p.token);
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
		debug!("try_parse_separator(cx, p @ {})", p.token);
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
		debug!("parse_repeat_range(cx, p @ {})", p.token);
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
		debug!("parse_numeric_range(cx, p @ {})", p.token);
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
		debug!("try_parse_uint(cx, p @ {})", p.token);
		match p.token {
			token::LIT_INTEGER(_) => Some(parse_uint(cx, p)),
			_ => None
		}
	}

	fn parse_uint(_: &mut ExtCtxt, p: &mut Parser) -> uint {
		debug!("parse_uint(cx, p @ {})", p.token);
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

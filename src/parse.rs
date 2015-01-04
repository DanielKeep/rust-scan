use std::borrow::ToOwned;

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
pub use self::scan_pattern::FormatSource;

pub use self::ScanArm::{FallbackArm, PatternArm};
pub use self::ArmTokenizer::{WordsAndInts, IdentsAndInts, SpaceDelimited, ExplicitTok};
pub use self::ArmWhitespace::{Ignore, ExplicitNewline, ExplicitSp, ExplicitAny, ExactSp};
pub use self::ArmCompareStrs::{CaseInsensitive, AsciiCaseInsensitive, ExactCS};

#[derive(Show)]
pub enum ScanArm {
	FallbackArm(Option<Spanned<ast::Ident>>),
	PatternArm(PatAst, Option<Spanned<ast::Ident>>, ScanArmAttrs),
}

#[derive(Show)]
pub struct ScanArmAttrs {
	pub pat_tok: ArmTokenizer,
	pub inp_tok: String,
	pub pat_sp: ArmWhitespace,
	pub inp_sp: String,
	pub pat_cs: ArmCompareStrs,
	pub inp_cs: String,
	pub trace: bool,
}

#[derive(Show)]
pub enum ArmTokenizer {
	WordsAndInts,
	IdentsAndInts,
	SpaceDelimited,
	ExplicitTok,
}

#[derive(Show)]
pub enum ArmWhitespace {
	Ignore,
	ExplicitNewline,
	ExplicitSp,
	ExplicitAny,
	ExactSp,
}

#[derive(Show)]
pub enum ArmCompareStrs {
	CaseInsensitive,
	AsciiCaseInsensitive,
	ExactCS,
}

pub fn parse_scan_arm(cx: &mut ExtCtxt, p: &mut Parser) -> (ScanArm, P<ast::Expr>) {
	debug!("parse_scan_arm(cx, p @ {})", p.token);
	let arm_attrs = parse_scan_arm_attrs(cx, p);
	let arm_pat = parse_scan_pattern(cx, p, arm_attrs);
	let arm_expr = parse_arm_expr(cx, p);
	(arm_pat, arm_expr)
}

fn parse_scan_arm_attrs(cx: &mut ExtCtxt, p: &mut Parser) -> ScanArmAttrs {
	debug!("parse_scan_arm_attrs(cx, p @ {})", p.token);

	fn lit_str<'a>(lit: &'a ast::Lit) -> Option<&'a str> {
		match &lit.node {
			&ast::LitStr(ref is, _) => Some(is.get()),
			_ => None
		}
	}

	let mut pat_tok = None;
	let mut inp_tok = None;
	let mut pat_sp = None;
	let mut inp_sp = None;
	let mut pat_cs = None;
	let mut inp_cs = None;
	let mut trace = None;

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
				inp_tok = Some(value.to_owned());
			},
			ast::MetaNameValue(ref name, ref value) if name.get() == "space" => {
				let value_span = value.span;
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value_span, "space must be a string"));

				let (sp_enum, sp_str) = match value {
					"Ignore" => (Ignore, "Ignore"),
					"ExplicitNewline" => (ExplicitNewline, "ExplicitNewline"),
					"Explicit" => (ExplicitSp, "Explicit"),
					"ExplicitAny" => (ExplicitAny, "ExplicitAny"),
					"Exact" => (ExactSp, "Exact"),
					_ => cx.span_fatal(value_span, "unrecognised space")
				};

				pat_sp = Some(sp_enum);
				if inp_sp.is_none() {
					inp_sp = Some(format!("rt::whitespace::{}", sp_str));
				}
			},
			ast::MetaNameValue(ref name, ref value) if name.get() == "runtime_sp" => {
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value.span, "runtime_sp must be a string"));
				inp_sp = Some(value.to_owned());
			},
			ast::MetaNameValue(ref name, ref value) if name.get() == "compare" => {
				let value_span = value.span;
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value_span, "compare must be a string"));

				let (cs_enum, cs_str) = match value {
					"CaseInsensitive" => (CaseInsensitive, "CaseInsensitive"),
					"AsciiCaseInsensitive" => (AsciiCaseInsensitive, "AsciiCaseInsensitive"),
					"Exact" => (ExactCS, "Exact"),
					_ => cx.span_fatal(value_span, "unrecognised compare")
				};

				pat_cs = Some(cs_enum);
				if inp_cs.is_none() {
					inp_cs = Some(format!("rt::compare_strs::{}", cs_str));
				}
			},
			ast::MetaNameValue(ref name, ref value) if name.get() == "runtime_cmp" => {
				let value = lit_str(value).unwrap_or_else(||
					cx.span_fatal(value.span, "runtime_cmp must be a string"));
				inp_cs = Some(value.to_owned());
			},
			ast::MetaWord(ref name) if name.get() == "trace" => {
				trace = Some(true);
			},
			_ => {
				cx.span_fatal(attr.span, "unrecognised attribute")
			}
		}
	}

	let pat_tok = pat_tok.unwrap_or(WordsAndInts);
	let inp_tok = inp_tok.unwrap_or("rt::tokenizer::WordsAndInts".to_owned());
	let pat_sp = pat_sp.unwrap_or(Ignore);
	let inp_sp = inp_sp.unwrap_or("rt::whitespace::Ignore".to_owned());
	let pat_cs = pat_cs.unwrap_or(CaseInsensitive);
	let inp_cs = inp_cs.unwrap_or("rt::compare_strs::CaseInsensitive".to_owned());
	let trace = trace.unwrap_or(false);

	ScanArmAttrs {
		pat_tok: pat_tok,
		inp_tok: inp_tok,
		pat_sp: pat_sp,
		inp_sp: inp_sp,
		pat_cs: pat_cs,
		inp_cs: inp_cs,
		trace: trace,
	}
}

fn parse_scan_pattern(cx: &mut ExtCtxt, p: &mut Parser, attrs: ScanArmAttrs) -> ScanArm {
	debug!("parse_scan_pattern(cx, p @ {}, {})", p.token, attrs);
	debug!("parse_scan_pattern(cx, p @ {})", p.token);
	fn parse_fallback_ident(_: &mut ExtCtxt, p: &mut Parser) -> Option<Spanned<ast::Ident>> {
		if p.eat(&token::Underscore) {
			None
		} else {
			Some(respan(p.span, p.parse_ident()))
		}
	}

	// This *might* be a fallback pattern.
	if p.token == token::Underscore {
		// ISSUE #15: currently, `_ => ()` is an anonymous fallback arm.  After a little while, change it so that it does an anonymous capture of a single *token*, rather than the whole line.
		if p.look_ahead(1, |t| *t == token::FatArrow) {
			p.fatal("in the future, a lone `_` will be an anonymous token capture; use `.._` instead.");
		}
	} else if p.token == token::DotDot {
		p.bump();
		let ident = parse_fallback_ident(cx, p);
		p.expect(&token::FatArrow);
		return FallbackArm(ident)
	}

	let pat_ast = scan_pattern::parse_pattern(cx, p);

	p.expect_one_of(&[], &[token::Comma, token::FatArrow]);
	match p.bump_and_get() {
		token::Comma => {
			// Has fallback.
			p.expect(&token::DotDot);
			let ident = parse_fallback_ident(cx, p);
			p.expect(&token::FatArrow);
			PatternArm(pat_ast, ident, attrs)
		},
		token::FatArrow => {
			// No fallback.
			PatternArm(pat_ast, None, attrs)
		},
		_ => panic!()
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
			&& p.token != token::CloseDelim(token::Brace);

		if require_comma {
			debug!("parse_arm_expr - require comma");
			p.commit_expr(&*arm_expr, &[token::Comma], &[token::CloseDelim(token::Brace), token::Eof]);
		} else {
			debug!("parse_arm_expr - don't require comma");
			p.eat(&token::Comma);
		}
	}

	debug!("parse_arm_expr - done");
	arm_expr
}

pub mod scan_pattern {
	pub use self::PatAst::{AstAlternates, AstSequence, AstText, AstRegex, AstOptional, AstCapture, AstSliceCapture, AstLookahead, AstRepetition};

	use std::borrow::ToOwned;
	use std::io;

	use syntax::ast;
	use syntax::codemap::{Spanned, respan};
	use syntax::parse;
	use syntax::parse::parser::Parser;
	use syntax::parse::token;
	use syntax::ptr::P;
	use syntax::ext::base::ExtCtxt;

	#[derive(Show)]
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
			node: Box<PatAst>,
			sep: Option<Box<PatAst>>,
			range: RepeatRange
		},
	}

	pub trait FormatSource {
		fn fmt_source(&self, w: &mut io::Writer) -> Result<(), io::IoError>;

		// Because I'm lazy and they removed `fmt::String` :'(
		fn to_source(&self) -> String {
			let mut buffer = vec![];
			self.fmt_source(&mut buffer).unwrap();
			String::from_utf8(buffer).unwrap()
		}
	}

	impl FormatSource for PatAst {
		fn fmt_source(&self, f: &mut io::Writer) -> Result<(), io::IoError> {
			match self {
				&AstAlternates(ref alts) => {
					try!(write!(f, "("));
					let mut alts = alts.iter();
					if let Some(alt) = alts.next() {
						try!(alt.fmt_source(f));
					}
					for alt in alts {
						try!(write!(f, "|"));
						try!(alt.fmt_source(f));
					}
					try!(write!(f, ")"));
				},
				&AstSequence(ref nodes) => {
					try!(write!(f, "("));
					let mut nodes = nodes.iter();
					if let Some(node) = nodes.next() {
						try!(node.fmt_source(f));
					}
					for node in nodes {
						try!(write!(f, " "));
						try!(node.fmt_source(f));
					}
					try!(write!(f, ")"));
				},
				&AstText(ref s) => {
					try!(write!(f, "\"{}\"", s.escape_default()));
				},
				&AstRegex(ref s) => {
					try!(write!(f, "/\"{}\"", s.escape_default()));
				},
				&AstOptional(ref node) => {
					try!(write!(f, "("));
					try!(node.fmt_source(f));
					try!(write!(f, ")?"));
				},
				&AstCapture(ref ident, ref m_ty) => {
					try!(write!(f, "{}", ident.node));
					if let Some(_) = *m_ty {
						// TODO: `{}` on a type is too verbose to be useful.
						try!(write!(f, ":$ty"));
					}
				},
				&AstSliceCapture(ref ident, ref node) => {
					try!(write!(f, "{}=", ident.node));
					try!(node.fmt_source(f));
				},
				&AstLookahead(ref node) => {
					try!(write!(f, "(?! "));
					try!(node.fmt_source(f));
					try!(write!(f, ")"));
				},
				&AstRepetition { ref node, ref sep, ref range } => {
					try!(write!(f, "["));
					try!(node.fmt_source(f));
					try!(write!(f, "["));
					if let Some(ref sep) = *sep {
						try!(sep.fmt_source(f));
					}
					try!(range.fmt_source(f));
				},
			}
			Ok(())
		}
	}

	#[derive(Show)]
	pub struct RepeatRange(pub uint, pub Option<uint>);

	impl FormatSource for RepeatRange {
		fn fmt_source(&self, f: &mut io::Writer) -> Result<(), io::IoError> {
			let &RepeatRange(n, m) = self;
			match (n, m) {
				(0, None) => try!(write!(f, "*")),
				(1, None) => try!(write!(f, "+")),
				(n, None) => try!(write!(f, "{{{},}}", n)),
				(0, Some(m)) => try!(write!(f, "{{,{}}}", m)),
				(n, Some(m)) if n == m => try!(write!(f, "{{{}}}", n)),
				(n, Some(m)) => try!(write!(f, "{{{},{}}}", n, m))
			}
			Ok(())
		}
	}

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
		while p.eat(&token::BinOp(token::Or)) {
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
				if p.eat(&token::Question) {
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
			token::Literal(token::Str_(ident), _) => {
				p.bump();
				Some(::syntax::parse::str_lit(ident.as_str()))
			},
			token::Literal(token::StrRaw(ident, _), _) => {
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
				if p.eat(&token::Question) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <regex> := "/" <text>
	fn try_parse_regex(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_regex(cx, p @ {})", p.token);
		if p.eat(&token::BinOp(token::Slash)) {
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
				if p.eat(&token::Question) {
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
			token::Ident(ident, _) => {
				let sp = p.span;
				p.bump();
				respan(sp, ident)
			},
			token::Underscore => {
				let sp = p.span;
				p.bump();
				respan(sp, cx.ident_of("_"))
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
		if p.eat(&token::Eq) {
			Some(parse_non_capture_atom(cx, p))
		} else {
			None
		}
	}

	// <constraint> := ":" <type>
	fn try_parse_constraint(_: &mut ExtCtxt, p: &mut Parser) -> Option<P<ast::Ty>> {
		debug!("try_parse_constraint(cx, p @ {})", p.token);
		if !p.eat(&token::Colon) {
			return None
		}

		Some(p.parse_ty())
	}

	// <maybe-group> := <group> "?"?
	fn try_parse_maybe_group(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_maybe_group(cx, p @ {})", p.token);
		try_parse_group(cx, p)
			.and_then(|node| {
				if p.eat(&token::Question) {
					Some(AstOptional(box node))
				} else {
					Some(node)
				}
			})
	}

	// <group> := "(" <alternates> ")"
	fn try_parse_group(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_group(cx, p @ {})", p.token);
		if !p.eat(&token::OpenDelim(token::Paren)) {
			return None
		}

		let lookahead = if p.eat(&token::Question) {
			p.expect(&token::Not);
			true
		} else {
			false
		};

		let node = parse_alternates(cx, p);
		p.expect(&token::CloseDelim(token::Paren));

		if lookahead {
			Some(AstLookahead(box node))
		} else {
			Some(node)
		}
	}

	// <repetition> := "[" <alternates> "]" <separator>? <repeat-range>
	fn try_parse_repetition(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_repetition(cx, p @ {})", p.token);
		if !p.eat(&token::OpenDelim(token::Bracket)) {
			return None
		}

		let node = parse_alternates(cx, p);
		p.expect(&token::CloseDelim(token::Bracket));

		let sep = try_parse_separator(cx, p).map(|n| box n);
		let range = parse_repeat_range(cx, p);

		Some(AstRepetition { node: box node, sep: sep, range: range })
	}

	// <separator> := "," | "." | ";" | ":" | <text> | <group>
	fn try_parse_separator(cx: &mut ExtCtxt, p: &mut Parser) -> Option<PatAst> {
		debug!("try_parse_separator(cx, p @ {})", p.token);
		match p.token {
			token::Comma => {
				p.bump();
				Some(AstText(",".to_owned()))
			},
			token::Dot => {
				p.bump();
				Some(AstText(".".to_owned()))
			},
			token::Semi => {
				p.bump();
				Some(AstText(";".to_owned()))
			},
			token::Colon => {
				p.bump();
				Some(AstText(":".to_owned()))
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
			token::Question => {
				p.bump();
				RepeatRange(0, Some(1))
			},
			token::BinOp(token::Star) => {
				p.bump();
				RepeatRange(0, None)
			},
			token::BinOp(token::Plus) => {
				p.bump();
				RepeatRange(1, None)
			},
			token::OpenDelim(token::Brace) => parse_numeric_range(cx, p),
			_ => {
				p.fatal("expected `?`, `*`, `+` or a numeric repeat range")
			}
		}
	}

	// <numeric-range> := "{" "," <uint> "}" | "{" <uint> ("," <uint>?)? "}"
	fn parse_numeric_range(cx: &mut ExtCtxt, p: &mut Parser) -> RepeatRange {
		debug!("parse_numeric_range(cx, p @ {})", p.token);
		p.expect(&token::OpenDelim(token::Brace));
		let (min, max) = match p.token {
			token::Comma => {
				// {,max} -> {0, max}
				p.bump();
				(0, Some(parse_uint(cx, p)))
			},
			token::Literal(token::Integer(_), _) => {
				let min = parse_uint(cx, p);
				let max = if p.eat(&token::Comma) {
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
		p.expect(&token::CloseDelim(token::Brace));

		RepeatRange(min, max)
	}

	fn try_parse_uint(cx: &mut ExtCtxt, p: &mut Parser) -> Option<uint> {
		debug!("try_parse_uint(cx, p @ {})", p.token);
		match p.token {
			token::Literal(token::Integer(_), _) => Some(parse_uint(cx, p)),
			_ => None
		}
	}

	fn parse_uint(_: &mut ExtCtxt, p: &mut Parser) -> uint {
		debug!("parse_uint(cx, p @ {})", p.token);
		let int_lit = match p.bump_and_get() {
			token::Literal(token::Integer(s), suffix) => {
				let suffix = suffix.as_ref().map(|s| s.as_str());
				parse::integer_lit(s.as_str(), suffix, &p.sess.span_diagnostic, p.span)
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

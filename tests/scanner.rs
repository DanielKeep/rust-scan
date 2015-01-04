#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

use std::borrow::ToOwned;
use self::Expr::{Lit, Add, Sub, Mul, Div};

#[derive(Show)]
struct Pair(pub int, pub int);

scanner! { Pair,
	x:int ".." => Pair(x, 0),
	x:int y:int => Pair(x, y),
}

#[test]
fn test_pair() {
	let res = scan! { "1 2 wat", p:Pair w:&str => format!("{} {}", p, w) };
	assert_eq!(res, Ok("Pair(1, 2) wat".to_owned()));

	let res = scan! { "3 .. huh", p:Pair w:&str => format!("{} {}", p, w) };
	assert_eq!(res, Ok("Pair(3, 0) huh".to_owned()));
}

#[derive(PartialEq, Show)]
struct V3 { pub x: f32, pub y: f32, pub z: f32 }

scanner! { V3,
    "<" x:f32 "," y:f32 "," z:f32 ">" => V3 { x:x, y:y, z:z }
}

#[test]
fn test_v3() {
	let res = scan! { "<1, 2, 3>", v:V3 => v };
	assert_eq!(res, Ok(V3 { x:1.0, y:2.0, z:3.0 }));
}

#[derive(Eq, PartialEq, Show)]
enum Expr {
	Lit(int),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
}

struct AddExpr(Expr);
struct MulExpr(Expr);
struct AtomExpr(Expr);

scanner! { AddExpr,
	lhs:MulExpr "+" rhs:AddExpr => {
		let (MulExpr(lhs), AddExpr(rhs)) = (lhs, rhs);
		AddExpr(Add(box lhs, box rhs))
	},
	lhs:MulExpr "-" rhs:AddExpr => {
		let (MulExpr(lhs), AddExpr(rhs)) = (lhs, rhs);
		AddExpr(Sub(box lhs, box rhs))
	},
	lhs:MulExpr => {
		let MulExpr(lhs) = lhs;
		AddExpr(lhs)
	}
}

scanner! { MulExpr,
	lhs:AtomExpr "*" rhs:MulExpr => {
		let (AtomExpr(lhs), MulExpr(rhs)) = (lhs, rhs);
		MulExpr(Mul(box lhs, box rhs))
	},
	lhs:AtomExpr "/" rhs:MulExpr => {
		let (AtomExpr(lhs), MulExpr(rhs)) = (lhs, rhs);
		MulExpr(Div(box lhs, box rhs))
	},
	lhs:AtomExpr => {
		let AtomExpr(lhs) = lhs;
		MulExpr(lhs)
	}
}

scanner! { AtomExpr,
	"(" expr:AddExpr ")" => {
		let AddExpr(expr) = expr;
		AtomExpr(expr)
	},
	lit:int => AtomExpr(Lit(lit))
}

scanner! { Expr,
	expr:AddExpr => {
		let AddExpr(expr) = expr;
		expr
	}
}

#[test]
fn test_llk_scanner() {
	assert_eq!(parse_expr("0"), Ok(
		Lit(0)
	));
	assert_eq!(parse_expr("42"), Ok(
		Lit(42)
	));
	assert_eq!(parse_expr("1+2"), Ok(
		Add(box Lit(1), box Lit(2))
	));
	assert_eq!(parse_expr("1+2*3+4"), Ok(
		Add(box Lit(1), box Add(box Mul(box Lit(2), box Lit(3)), box Lit(4)))
	));
	assert_eq!(parse_expr("(1+2*3)+4"), Ok(
		Add(box Add(box Lit(1), box Mul(box Lit(2), box Lit(3))), box Lit(4))
	));
	assert_eq!(parse_expr("2*(1+3)"), Ok(
		Mul(box Lit(2), box Add(box Lit(1), box Lit(3)))
	));
	assert_eq!(parse_expr("0 0"), Err(
		scan_util::OtherScanError("expected end of input, got `0`".to_owned(), 1)
	));
	assert_eq!(parse_expr("1 + (2 | 3)"), Err(
		scan_util::OtherScanError("expected end of input, got `+`".to_owned(), 1)
	));
}

fn parse_expr(s: &str) -> Result<Expr, scan_util::ScanError> {
	scan!{ s, e:Expr => e }
}

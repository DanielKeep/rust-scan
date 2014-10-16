#![feature(phase)]

#[phase(plugin)] extern crate regex_macros;
extern crate regex;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("a b c d e", Ok("a.b.c.d.e")),
	("a/c/e", Ok("a./.c./.e")),
	("a0c1e.f", Ok("a.0.c1e...f")),
];

#[test]
fn issue_11() {
	for &(inp, exp) in TEST_PAIRS.iter() {
		let got = do_scan(inp);
		match exp {
			Ok(exp_str) => assert_eq!(got, Ok(exp_str.into_string())),
			Err(()) => assert!(got.is_err()),
		}
	}
}

fn do_scan(s: &str) -> Result<String, scan_util::ScanError> {
	scan! {
		s,
		a:&str b:&str c=/r"[a-zA-Z_][a-zA-Z0-9_]*" d:&str e:&str => format!("{}.{}.{}.{}.{}", a, b, c, d, e)
	}
}

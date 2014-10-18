#![feature(phase)]

#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("abc", Ok("one token")),
	("abc def", Ok("two tokens")),
	("abc def ghi jkl", Err(())),
	("[1 2 3 4]", Ok("int list")),
	("x wat", Ok("xyz")),
	("y moo", Ok("xyz")),
	(" y ", Ok("xyz")),
	("z 47", Ok("xyz")),
];

#[test]
fn issue_15() {
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
		"[" [_:int _:int]+ "]" => format!("int list"),
		"x" _:&str | "y" (_:&str)? | _="z" (_:&str | _:&str) => format!("xyz"),
		_:&str => format!("one token"),
		_:&str _:&str => format!("two tokens"),
	}
}

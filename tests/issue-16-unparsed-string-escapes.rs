#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("\"x\"", Ok("x")),
	("x\\\\x", Ok("xx")),
];

#[test]
fn issue_16() {
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
		"\"" x "\"" => x,
		x0:String r#"\\"# x1:String => x0+x1,
	}
}

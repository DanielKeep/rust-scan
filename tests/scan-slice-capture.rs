#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("nums (1 2 3)", Ok("1 2 3")),
	("nums ( 1 2 3 )", Ok("1 2 3")),
	("nums ( \t1   2\t 3  )", Ok("1   2\t 3")),
	("yes", Ok("yes")),
	(" yes", Ok("yes")),
	("yes ", Ok("yes")),
	(" yes ", Ok("yes")),
	("no", Ok("no")),
];

#[test]
fn test_scan() {
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
		"nums (" nums=[_:int]* ")" => nums.into_string(),
		yesno = ("yes" | "no") => yesno.into_string(),
	}
}

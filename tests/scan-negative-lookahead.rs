#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("words 'a b c'", Ok("[a, b, c]")),
	("words 'a b c", Err(())),
	// for sinistersnare:
	("def add x y: x y + end", Ok("def add [x, y]: [x, y, +] end")),
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
		"words '" [(?!"'") ws:&str]* "'" => format!("{}", ws),

		#[tokenizer="IdentsAndInts"]
		"def" name:&str [(?!":") args:&str]* ":" [(?!"end") body_toks:&str]* "end" => {
			format!("def {} {}: {} end", name, args, body_toks)
		},
	}
}

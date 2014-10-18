#![feature(phase)]

#[phase(plugin)] extern crate regex_macros;
extern crate regex;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("oHaI", Ok("oHaI")),
	("OHaI", Ok("ohai")),
	("ohaI", Ok("ohai")),
	("oHAI", Ok("ohai")),
	("oHai", Ok("ohai")),
	("sσ", Ok("exact sσ")),
	("Sσ", Ok("ascii sσ")),
	("sΣ", Ok("unico sσ")),
	("SΣ", Ok("unico sσ")),
];

#[test]
fn issue_18() {
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
		#[compare="Exact"] "oHaI" => "oHaI".into_string(),
		#[compare="CaseInsensitive"] "ohai" => "ohai".into_string(),
		#[compare="Exact"] "sσ" => "exact sσ".into_string(),
		#[compare="AsciiCaseInsensitive"] "sσ" => "ascii sσ".into_string(),
		#[compare="CaseInsensitive"] "sσ" => "unico sσ".into_string(),
	}
}

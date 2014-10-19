#![feature(phase)]

#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	(" ! \t @\n#\t\r\n $  ", Ok("exact")),
	("\t! @\r\n# \n\t$\t", Ok("exp")),
	("!@\n#\n$", Ok("exp_nl")),
	("\n!\r\n@ # $\n", Ok("exp_any")),
	("!@#$", Ok("ignore")),
];

#[test]
fn issue_4() {
	for (i,&(inp, exp)) in TEST_PAIRS.iter().enumerate() {
		info!("Running test set {}: `{}`, expecting {}", i, inp.escape_default(), exp.clone().map(|s| s.escape_default()));
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
		#[trace]#[space="Exact"]			" ! \t @\n#\t\r\n $  " => "exact".into_string(),
		#[trace]#[space="Explicit"]			" ! \t @\n#\t\r\n $  " => "exp".into_string(),
		#[trace]#[space="ExplicitNewline"]	" ! \t @\n#\t\r\n $  " => "exp_nl".into_string(),
		#[trace]#[space="ExplicitAny"]		" ! \t @\n#\t\r\n $  " => "exp_any".into_string(),
		#[trace]#[space="Ignore"]			" ! \t @\n#\t\r\n $  " => "ignore".into_string(),
	}
}

#![feature(phase)]

#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate regex_macros;
extern crate regex;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

#[test]
fn test_trace() {
	do_scan("hi there");
	do_scan("bye");
	do_scan("not a chance");
}

fn do_scan(s: &str) {
	let _ = scan! { s,
		#[trace] "hi" ("there"|"!")? => (),
		#[trace] /"z{4,7}" => (),
		#[trace] "bye" ("now"|".")? => (),
		#[trace] "not" [(?!"even") word=/r"[^\s]+"]{2} => (),
	};
}

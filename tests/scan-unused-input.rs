#![feature(phase)]
#![forbid(unused_variable)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

#[test]
fn test_scan_unused() {
	let _ = scan! { "x", _ => () };
}

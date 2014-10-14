#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

use scan_util::ScanError;

#[test]
fn test_match_block() {
	match scan!{ "8", x:int => x } {
		Ok(x) => assert_eq!(x, 8),
		Err(err) => {
			let _: ScanError = err;
			assert!(false);
		}
	}
}

#[test]
fn test_try() {
	let blk = || -> Result<int, ScanError> {
		Ok(try!(scan! { "8", x:int => x }))
	};

	assert_eq!(blk(), Ok(8));
}

#[test]
fn test_and_then() {
	let x = (scan! { "8", x:int => x })
		.and_then(|x| Ok(x*2))
		.unwrap_or_else(|_| { assert!(false); 0 });
	assert_eq!(x, 16);
}

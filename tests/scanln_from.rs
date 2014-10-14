#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

use std::io::BufReader;

const INPUT: &'static str = r###"don't bring me down
until the end of the line"###;

#[test]
fn test_scanln_from() {
	let mut input_reader = BufReader::new(INPUT.as_bytes());

	match {
		scanln_from! {
			&mut input_reader,
			("don't" neg:())? "bring me" dir:&str => (neg, dir.into_string())
		}
	} {
		Ok((Some(()), ref dir)) if dir.as_slice() == "down" => (),
		other @ _ => assert!(false, "1st, got {}", other)
	}

	match {
		scanln_from! {
			&mut input_reader,
			"until the end of the line" => ()
		}
	} {
		Ok(()) => (),
		other @ _ => assert!(false, "2nd, got {}", other)
	}
}

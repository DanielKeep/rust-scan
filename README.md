# rust-scan

A pattern-based `scanf` alternative for Rust.

This is still a work-in-progress.

## Usage

To use this, add the following to your project's `Cargo.toml` file:

	[dependencies.scan]

	git = "https://github.com/DanielKeep/rust-scan.git"

	[dependencies.scan_util]

	git = "https://github.com/DanielKeep/rust-scan-util.git"

Then, in your project's root module:

	#![feature(phase)]

	#[phase(plugin)] extern crate scan;
	extern crate scan_util;

Note that although you can rename `scan`, you *must not* rename `scan_util`, or the macros won't work.

## Example

The following shows how to process a line from standard input in various ways:

	#![feature(phase)]

	#[phase(plugin)] extern crate scan;
	extern crate scan_util;

	fn main() {
		print!("input> ");

		let res = scanln! {
			// Match a sequence of three tokens: "such", "input" and "!".
			"such input!" => "very syntax".into_string(),

			// Match *five* tokens: "and", "now", ".", "." and ".".
			"and now..." => "...for something completely different".into_string(),

			// Match "has" followed by an integer, captured into "value".
			"has" value:int => format!("ok has {}", value),

			// Match "also", with the remainder of the input captured by "tail".
			"also", ..tail => format!("also, also, {}", tail.trim_left()),

			// If no other arm else matches, capture all the input into "tail".
			// You could also use `_` as the pattern, if you don't care about the
			// actual input.
			..tail => format!("wow {}", tail),
		};

		match res {
			Ok(value) => println!("Ok: {}", value),
			Err(err) => println!("Err: {}", err)
		}
	}

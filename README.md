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
		loop {
			print!("input> ");

			let res = scanln! {
				// Match a single word.
				"hi" => "Hi!".into_string(),

				// Match a sequence of three tokens: "such", "input" and "!".
				"such input!" => "very syntax".into_string(),

				// Match *five* tokens: "and", "now", ".", "." and ".".
				"and now..." => "...for something completely different".into_string(),

				// Match "has" followed by an integer, captured into "value".
				"has" value:int => format!("ok has {}", value),

				// Alternative to the above.
				"has also" value => {
					let value: int = value;
					format!("ok has also {}", value)
				},

				// Match "v" followed by three numbers.
				"v" x:f64 y:f64 z:f64 => format!("({}, {}, {})", x, y, z),

				// Match "rgb" followed by three numbers.
				"rgb" r:u8 g:u8 b:u8 => format!("#{:02x}{:02x}{:02x}", r, g, b),

				// Match "rgb vec" followed by three or four numbers.
				"rgb vec" [rgb:u8],{3,4} => format!("rgba{}", rgb),

				// Match "vecs" followed by a comma-delimited vector of
				// comma-delimited vectors of floats.
				("vecs"|"vechs"|"vetches") [ "[" [vss:f64],* "]" ],* => format!("vss: {}", vss),

				// Match a vector of either an int or a word.
				"ints and words:" [is:int | ss:&str]* => format!("is: {}, ss: {}", is, ss),

				// Match a list of words separated by "and" or a comma.
				"words:" [words:&str]("and"|", and"|",")+ => format!("words: {}", words),

				// Match a list of numbers separated by words.
				"int words:" [is:int](ss:&str)* => format!("is: {}, ss: {}", is, ss),

				// Match "go" followed by a single word.
				"go" exit:&str => format!("Leaving by the {} exit.", exit),

				// Match "yes" or "no".
				"yes" | "no" => "maybe yup".into_string(),

				// Match an integer or a float.
				"i" i:int | "f" f:f64 => format!("i {} | f {}", i, f),

				// Match "identify", with the remainder of the input captured by "tail".
				"identify", ..tail => identify_banana(tail).into_string(),

				// Exit this loop.
				"exit" | "quit" => break,

				// If no other arm else matches, capture all the input into "tail".
				// You could also use `_` as the pattern, if you don't care about the
				// actual input.  Note that this will also swallow any errors caused
				// by previous arms not matching.
				//..tail => format!("wow {}", tail),
			};

			match res {
				Ok(value) => println!("Ok: {}", value),
				Err(err) => println!("Err: {}", err)
			}
		}
	}

	fn identify_banana<S: Str>(s: S) -> &'static str {
		(scan! {
			s,
			"banana" => "definitely a banana",
			"crowbar" => "giant metal banana",
			"human finger" => "tiny crunchy banana",
			_ => "probably not a banana"
		}).unwrap()
	}

/*!
Pattern-based input scanning.

## Setup

First, place the following in your `Cargo.toml` file:

```{notrust}
[dependencies.scan]

git = "https://github.com/DanielKeep/rust-scan.git"

[dependencies.scan_util]

git = "https://github.com/DanielKeep/rust-scan-util.git"
```

If you are not using Cargo, you will need to download and compile both of these crates and make them available to `rustc`.  Note that the `scan` crate is *only* needed at compile time and does not need to be distributed.

Then, in your root module, add the following:

```{ignore}
#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;
```

Note that `scan_util` *must* be in the root module, and *must* not be renamed.  This is due to a limitation of macros in Rust.

## Usage

Two macros are provided for scanning text.  In general, both are concerned with taking a string and matching it against a sequence of patterns.  As soon as a matching pattern is found, the corresponding expression is evaluated and returned.

If an error occurs during an underlying IO operation *or* none of the provided patterns matched, an `Err(scan_util::ScanError)` will be returned.

The `scanln!` macro is used to scan a line of input from standard input.  For example:

```{ignore}
scanln! {
	"say hello" => println!("Hi!"),
	"exit" => return,
}
```

The `scan!` macro is used to scan a block of text.  The input value must implement the `Str` trait.  For example:

```{ignore}
let noun_to_identify = "wheelbarrow";
scan! {
	noun_to_identify,

	"banana" => "definitely a banana",
	"crowbar" => "big metal banana",
	_ => "probably not a banana at all",
}
```

## Patterns

A pattern is a space-separated sequence of one or more of the following:

### Text

These indicate a block of text which should be matched.  This matching is done by converting *both* text in the pattern *and* input text into a sequence of tokens, ignoring whitespace.  Each token is either a sequence of letters, a sequence of digits, or a single non-whitespace character.  All whitespace is ignored.

For example, the string `"Left4Dead is pretty fun..."` is tokenised into the sequence `"Left"`, `"4"`, `"Dead"`, `"is"`, `"pretty"`, `"fun"`, `"."`, `"."`, and `"."`.

This behaviour also means that `"ab cd ef"` and `"ab" "cd" "ef"` are semantically equivalent, but `"ab c" "d ef"` is different, because of the break between `c` and `d`.

Also note that text comparisons are *case insensitive*.

In future, the tokeniser, whitespace policy, and case insensitivity will be configurable.

### Capture

These are used to capture a value from the input.  They are written in the form `name: type` like a `let` binding.  The type constraint (`:type`) may be omitted *if* the usage of the captured value allows the type to be inferred.

For example, `age:int` will capture an integer and store it in a binding called `age`.  `thingy` will capture a value, the type of which is to be determined by inference.

An additional restriction is that you can only capture types which implement the `scan_util::Scanner` trait.  By default, scanners for the builtin integer and floating-point types are available as well as `bool` and `char`.  There is also a scanner for `&str` which will capture a single token.

If you wish to supply your own scanning logic for a type, you will need to wrap it in another type (such as a structure) and implement the `Scanner` trait.

### Alternate

A pattern written as `"a" | "b"` will match *either* `a` or `b`.  You can chain as many alternates as you want.

Note that alternates are tried in lexical order, and the first one to match "wins" and no others are tried.  For example, in the pattern `"a" | "a b"`, the second branch will never match.  Also note that as the scanner does not do backtracking, a failure to match later on in a pattern will not cause other alternatives to be tried.

All captures inside an alternate are wrapped in `Option`, with unmatched captures being `None`.

### Group

You can create a sub pattern using parentheses.  This is mainly used for grouping alternates.  For example: `"a" ("b" | "c") "d"`; this will match `a b d` or `a c d`.

### Repetition

Reptitions are used to match a repeating sequence, and optionally capture values from said sequence.  The general form is `[pattern] separator repeat`.

For example:

* `["pretty"]+ "please"` - matches a childish plea.

* `"rgb (" [cs:f32],{3} ")"` - matches a CSS `rgb` colour value.

* `[ns:int](ws:&str)+` - matches a sequence of at least one number separated by arbitrary words.

The `separator` is used to indicate what to match between instances of the main repeating `pattern`.  It can be omitted (in which case no seperator is scanned), or a sub pattern.  A sub pattern consisting of a single text literal can be written with the parentheses omitted.  If the text literal is `","`, `"."`, `";"`, or `":"`, then it may be written without the quotes as well.

The `repeat` indicates how many times the repeating `pattern` must appear.  It can be any one of:

- `?` - zero or one times.

- `*` - zero or more times.

- `+` - one or more times.

- `{n}` - exactly `n` times.

- `{n,}` - at least `n` times.

- `{,m}` - zero or more, but no more than `m`, times.

- `{n,m}` - at least `n`, but no more than `m`, times.

All captures inside a repetition are inserted into `Vec`s.  Nesting repetitions yield nested `Vec`s.

### Tail Capture

Patterns must match their *entire* input.  If you only wish to perform a prefix match, you must add a tail capture to the pattern.  For example, `"spake thee thus", ..the_rest` would match "spake thee thus" and then capture the remainder of the input into `the_rest`.  Note the comma to distinguish the tail capture from the pattern.  Also note that tail captures are `&str`s, limited to the lifetime of the input.

If you wish to do a prefix match, but do not actually care about the remainder of the input, you can use `_` as the capture name.

### Fallback Pattern

Finally, you can specify a fallback arm which *always* matches.  The complete pattern should be either a tail capture (such as `..tail` without any preceeding pattern), or a simple `_`.

## Example

Below is a complete example of various kinds of patterns.

```{ignore}
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
```
*/
#![feature(phase)]
#![feature(plugin_registrar)]
#![feature(quote)]
#![feature(struct_variant)]

#[phase(plugin, link)] extern crate log;
extern crate rustc;
extern crate syntax;

extern crate scan_util;

pub mod parse;
pub mod plugin;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
	reg.register_macro("scan", plugin::expand_scan);
	reg.register_macro("scanln", plugin::expand_scanln);
	reg.register_macro("scanln_from", plugin::expand_scanln_from);
}

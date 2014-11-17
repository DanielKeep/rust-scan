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

Several macros are provided for scanning text.  In general, they are concerned with taking a string and matching it against a sequence of patterns.  As soon as a matching pattern is found, the corresponding expression is evaluated and returned.

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
	.._ => "probably not a banana at all",
}
```

`scanln_from!` provides a way to scan a line of text from a source *other* than standard input.

```{ignore}
scanln_from! {
	some_reader,

	"say hello" => println!("Hi from a reader!"),
	"exit" => return,
}
```

Finally, you can implement custom scanners for any type using the `scanner!` macro.

```{ignore}
struct Boolish(pub bool);

scanner! {
	Boolish,

	"true" | "yes" | "on" | "1" | "はい" => Boolish(true),
	"false" | "no" | "off" | "0" | "いいえ" => Boolish(false),
}
```

There are some illustrative examples below.  There are also some larger demonstrative examples:

- [`rust-scan-calc`](https://github.com/DanielKeep/rust-scan-calc) is a simple calculator that shows how you can parse infix arithmetic expressions.
- [`rust-scan-wavefront`](https://github.com/DanielKeep/rust-scan-wavefront) is a Wavefront OBJ lexer.

If you want more, the integration tests (in the `tests` directory) function as examples for (hopefully) every available feature.

## Example

Below is an example of various kinds of patterns.

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
		["na"]{8} => "Batman!",
		_ => "probably not a banana"
	}).unwrap()
}
```

## Patterns

A pattern is a sequence of one or more of the elements in the following sub-sections.

Here is a quick summary of the available features:

- Sequencing: `"set" n:int "."`.
- Literal text: `"hi"` `"two words"` `"by default,   whitespace\tis\r\n  and\tcase\nare ignored."`.
- Regular expressions: `/"[0-9]+"`.
- Captures: `"vec3d:" x:f32 "," y:f32 "," z:f32`.
- Slice captures: `"a string literal:" literal=/r#"^\"(\\\.|.)*\""#`.
- Alternates: `"a or b:" ("a" | "b")`.
- Repetition: `"usernames:" [names]+`.
- Negative Lookahead Assertions: `"some words:" [(?! ".") anything]* "."`.
- Tail capture: `"user-agent:", ..the_rest`.

### Sequence

Writing two or more matches side-by-side separated by whitespace causes them to be matched one after another, in turn.  For example, a pattern which matches the literal word `set`, captures an integer, then matches a full stop would be written as: `"set" n:int "."`.

Note that the whitespace between pattern elements is ignored and is only *actually* required if its removal would change the pattern's meaning.

### Text

String literals indicate a block of text which should be matched.  This matching is done by converting *both* text in the pattern *and* input text into a sequence of tokens.  The precise semantics of this can be controlled using the `#[tokenizer="..."]` and `#[space="..."]` attributes (see below).

For example, the string `"Left4Dead is pretty fun..."` is tokenised (under default settings) into the sequence `"Left"`, `"4"`, `"Dead"`, `"is"`, `"pretty"`, `"fun"`, `"."`, `"."`, and `"."`.

Note that although you can have adjacent string literals in a pattern, they *are not* glued together prior to being tokenised.  This means that, in a pattern, `"ab cd ef"` and `"ab" "cd" "ef"` are semantically equivalent, but `"ab c" "d ef"` is distinct from them, because of the break between `c` and `d`.

Also note that text comparisons are *case insensitive* by default.  This can be overriden using the `#[compare="..."]` attribute (see below).

### Regular Expression

If you need more control, you can also match against an arbitrary regular expression, as supported by the `regex` crate.  A regex is written as `/str`, where `str` is a string literal.  In most cases, you will want to use a raw string literal, which would look like `/r"..."`, or `/r##"..."##`.

A regular expression is matched against a slice of the input *starting* at the current scan position, with all leading whitespace skipped, *until* the end of input.  In order for a regex to "match", it must find a match at the very beginning of this slice.  You can make matches faster by beginning your regex with `^`.

For example, `"a" /"[0-9]+" "b"` will match "a 0 b", "a 42 b", and "a42b" (due to the default tokeniser).

### Capture

These are used to capture a value from the input.  They are written in the form `name: type` like a `let` binding.  The type constraint (`:type`) may be omitted *if* the usage of the captured value allows the type to be inferred.

For example, `age:int` will capture an integer and store it in a binding called `age`.  `thingy` will capture a value, the type of which is to be determined by inference.

An additional restriction is that you can only capture types which implement the `scan_util::Scanner` trait.  By default, scanners for the builtin integer and floating-point types are available as well as `bool` and `char`.  There are also scanners for `&str` and `String` which will capture a single token.

You can use the `scanner!` macro to implement a custom scanner using the same scan pattern syntax as the other macros.  Alternately, you can implement the `Scanner` trait manually (see the documentation for the `scan_util` crate).

### Slice Capture

You can also capture the slice of the input which corresponds to a particular sub-pattern.  This is written as `name = pattern`.  For example, given the pattern `"a" bc=(["b"]+ | "c") "d"`, scanning `"a b   b \t b c"` will bind `"b   b \t b"` to `bc`.

This is particularly useful in conjunction with alternates containing simple token matches (such as `("yes" | "no")`) and regular expressions.

### Alternate

A pattern written as `"a" | "b"` will match *either* `a` or `b`.  You can chain as many alternates as you want.

Note that alternates are tried in lexical order, and the first one to match "wins" and no others are tried.  For example, in the pattern `"a" | "a b"`, the second branch will never match due to ordering.  Also note that as the scanner does not do backtracking, a failure to match later on in a pattern will not cause other alternatives to be tried.

All captures inside an alternate are wrapped in `Option`, with unmatched captures being `None`.  This is true even if a capture is present in *all* alternates.

### Group

You can create a sub pattern using parentheses.  This is mainly used for grouping alternates.  For example: `"a" ("b" | "c") "d"`; this will match `a b d` or `a c d`.

### Repetition

Reptitions are used to match a repeating sequence, and optionally capture values from said sequence.  The general form is `[pattern] separator repeat`.

For example:

* `["pretty"]+ "please"` - matches a childish plea.

* `"rgb (" [cs],{3} ")"` - matches a CSS `rgb` colour value, with comma-separated values.

* `[ns:int](ws:&str)+` - matches a sequence of at least one number separated by arbitrary words, with both the numbers *and* words being captured.

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

Note that the *entirety* of `pattern` must match for a specific iteration to "count".  If it only *partially* matches, then the scan cursor will be rolled back to the beginning of that specific iteration, and the repetition as a whole will end.

### Negative Lookahead Assertion

Because `scan` does not perform backtracking, the pattern `"a" [words] "b"` *cannot* be matched: the `[words]` repetition will *always* consume the ending `"b"` token.

To get around this, you can use a negative lookahead assertion.  We can fix this example by rewriting it as `"a" [(?!"b") words] "b"`.  The repetition's pattern now reads as "match `words`, unless the next token matches `"b"`."

More specifically, a NLA is written as `(?! pattern)`.  It *never* consumes any of the input, and only matches if the given `pattern` *does not* match.  As demonstrated above, this construct exists to help terminate repetitions.

As an aside, NLAs are not always necessary.  For example, `"a" [words ints] "b"` does not need a NLA.

### Tail Capture

With the exception of the `scanner!` macro, patterns must match their *entire* input.  If you only wish to perform a prefix match, you must add a tail capture to the pattern.  For example, `"spake thee thus", ..the_rest` would match "spake thee thus" and then capture the remainder of the input into `the_rest`.  Note the comma to distinguish the tail capture from the pattern.  Also note that tail captures are `&str`s, limited to the lifetime of the input.

If you wish to do a prefix match, but do not actually care about the remainder of the input, you can use `_` as the capture name.

### Fallback Pattern

Finally, you can specify a fallback arm which *always* matches.  The complete pattern should be a tail capture (such as `..tail` without any preceeding pattern).

### Pattern Attributes

There are several attributes you can set on individual patterns.  They can be written in any order prior to the start of a pattern.

#### `#[tokenizer="..."]` and `#[runtime_tok="..."]`

These set the tokenisers used to turn string literals in the pattern *and* the input string into a sequence of tokens.  Setting `#[tokenizer="..."]` automatically sets `#[runtime_tok="..."]` to the same tokeniser by default, although this can be overridden by setting `#[runtime_tok="..."]` explicitly.  In the case of the input string, tokenisation happens under two circumstances:

1. When the pattern wants to match a literal token, the next token from the input string is popped and compared.
2. When a type's scanner requests the next token.

Currently, the second only happens with the `&str` and `String` scanners.

You can set any of the following values:

* `WordsAndInts` (default) - each token is *either* one or more characters with the `Alphabetic` Unicode property *or* one or more characters in the "Number" Unicode category.
* `IdentsAndInts` - each token is *either* a Rust identifier *or* one or more characters in the "Number" category.
* `SpaceDelimited` - each token is one or more characters which *do not* have the `White_Space` property.
* `Explicit` - each quoted string literal is turned into a single token.  In the case of input, the *entire input string* becomes a single token.

For `#[runtime_tok="..."]` you can also specify a path to a value or type that implements the `scan_util::Tokenizer` trait.

#### `#[space="..."]` and `#[runtime_sp="..."]`

These set the whitespace policies used for patterns and on input strings.  They control what whitespace to ignore, and what to turn into explicit tokens that must be matched.  The available policies are:

* `Ignore` (default) - whitespace between tokens is ignored.
* `ExplicitNewline` - end of line sequences are turned into an explicit token; all other whitespace is ignored.
* `Explicit` - runs of whitespace are turned into a single `" "` token, individual end of line sequences are turned into `"\n"` tokens.
* `ExplicitAny` - runs of whitespace and end of line sequences are all turned into a single explicit `" "` token.
* `Exact` - all whitespace characters are turned into literal tokens.

For `#[runtime_sp="..."]` you can also specify a path to a value or type that implements the `scan_util::Whitespace` trait.

### `#[compare="..."]` and `#[runtime_cmp="..."]`

These set the comparators used to determine if two strings "match".  The available comparators are:

* `CaseInsensitive` (default) - ignore case differences between strings.  Unicode normalisation is *not* done.  Note that this is *not* currently correct; it works by lowercasing individual code points and does not account for code points that map to multiple code points when lowercased.  It also does not take the user's locale into account.  As such, the exact behaviour of this setting may change in the future.
* `AsciiCaseInsensitive` - ignore case differences between strings for code points in the ASCII range.  Unicode normalisation is *not* done.
* `Exact` - strings will be compared for exact, binary equality.  Unicode normalisation is *not* done.

For `#[runtime_cmp="..."]` you can also specify a path to a value or type that implements the `scan_util::CompareStrs` trait.

**Note**: currently, `#[compare="..."]` does not do anything, as tokens are never compared at compile-time.

### Pattern Grammar

The following is the grammar used to parse patterns.

```{notrust}
<pattern> := <attributes> <alternates>

<attributes> := <attribute> <attributes>?

<alternates> := <sequence> ("|" <alternates>)?

<sequence> := <sequence-atom> <sequence-atom>?

<sequence-atom> := <maybe-capture> | <non-capture-atom>

<non-capture-atom> := <maybe-text> | <maybe-regex> | <maybe-group> | <repetition>

<maybe-text> := <text> "?"?

<text> := <string_literal> | <raw_string_literal>

<maybe-regex> := <regex> "?"?

<regex> := "/" <text>

<maybe-capture> := <capture> "?"?

<capture> := <identifier> ( <slice-capture> | <constraint>? )

<slice-capture> := "=" <non-capture-atom>

<constraint> := ":" <type>

<maybe-group> := <group> "?"?

<group> := "(" <lookahead>? <alternates> ")"

<lookahead> := "?" "!"

<repetition> := "[" <alternates> "]" <separator>? <repeat-range>

<separator> := "," | "." | ";" | ":" | <text> | <group>

<repeat-range> := "?" | "*" | "+" | <numeric-range>

<numeric-range> := "{" "," <uint> "}" | "{" <uint> ("," <uint>)? "}"
```
*/
#![feature(if_let)]
#![feature(phase)]
#![feature(plugin_registrar)]
#![feature(quote)]

#[phase(plugin, link)] extern crate log;
extern crate rustc;
extern crate syntax;

extern crate scan_util;

mod parse;
mod plugin;

mod util;

#[doc(hidden)]
#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
	reg.register_macro("scan", plugin::expand_scan);
	reg.register_macro("scanln", plugin::expand_scanln);
	reg.register_macro("scanln_from", plugin::expand_scanln_from);
	reg.register_macro("scanner", plugin::expand_scanner);
}

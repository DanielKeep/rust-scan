#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

const TEST_PAIRS: &'static [(&'static str, Result<&'static str, ()>)] = &[
	("a b c", Ok("a b c")),
	("a b", Err(())),
	("a b c d", Err(())),
	("n 0", Ok("n 0")),
	("n 1", Ok("n 1")),
	("n 9", Ok("n 9")),
	("n 10", Ok("n 10")),
	("n", Err(())),
	("n x", Err(())),
	("n 0x", Err(())),
	("s zyzzy", Ok("s zyzzy")),
	("s  zyzzy ", Ok("s zyzzy")),
	("s 42", Ok("s 42")),
	("s xy34", Err(())),
	("s", Err(())),
	("s(", Ok("s (")),
	("s( )", Err(())),
	("s(zyzzy)", Ok("s(zyzzy)")),
	("banana", Err(())),
	("", Err(())),

	("hi", Ok("Hi!")),
	("such input!", Ok("very syntax")),
	("and now...", Ok("...for something completely different")),
	("and now . . .", Ok("...for something completely different")),
	("has 42", Ok("ok has 42")),
	(" has 123 ", Ok("ok has 123")),
	(" has -1701 ", Ok("ok has -1701")),
	("has also 51", Ok("ok has also 51")),
	("v 1 2 3", Ok("(1, 2, 3)")),
	("v 1.0 2.5 3.75", Ok("(1, 2.5, 3.75)")),
	("rgb 25 82 195", Ok("#1952c3")),
	("rgb 25 82 256", Err(())),
	("rgb 25 82", Err(())),
	("rgb 25 82 195 72", Err(())),
	("rgb vec 45, 201, 72", Ok("rgba[45, 201, 72]")),
	("rgb vec 45, 201, 72, 64", Ok("rgba[45, 201, 72, 64]")),
	("rgb vec 45, 201", Err(())),
	("rgb vec 45, 201, 72, 64, 12", Err(())),
	("vecs", Ok("vss: []")),
	("vecs []", Ok("vss: [[]]")),
	("vecs [], []", Ok("vss: [[], []]")),
	("vecs [1, 2, 3]", Ok("vss: [[1, 2, 3]]")),
	("vecs [1, 2, 3], []", Ok("vss: [[1, 2, 3], []]")),
	("vecs [1, 2, 3], [4, 5, 6]", Ok("vss: [[1, 2, 3], [4, 5, 6]]")),
	("vechs [1, 2, 3], [4, 5, 6]", Ok("vss: [[1, 2, 3], [4, 5, 6]]")), // yiss
	("vetches [1, 2, 3], [4, 5, 6]", Ok("vss: [[1, 2, 3], [4, 5, 6]]")), // ho-kay
	("vecs [", Err(())),
	("vecs [] []", Err(())),
	("vecs [1, 2 3]", Err(())),
	("vecs [1, 2, 3], [", Err(())),
	("vecs [1, 2, 3,, 4, 5, 6]", Err(())),
	("ints and words: 47 21 dog 69 Belgium rutabaga", Ok("is: [Some(47), Some(21), None, Some(69), None, None], ss: [None, None, Some(dog), None, Some(Belgium), Some(rutabaga)]")),
	("words: I and you, there, what and ?, and splang", Ok("words: [I, you, there, what, ?, splang]")),
	("int words: 42 quickly 8 all -21 bears 22", Ok("is: [42, 8, -21, 22], ss: [quickly, all, bears]")),
	("go away", Ok("Leaving by the away exit.")),
	("go 42", Ok("Leaving by the 42 exit.")),
	("goto some_bigStupid_1d3nt1f13r", Ok("Sending attack raptor to some_bigStupid_1d3nt1f13r.")),
	("go-to where-the.Sun!don't_Sh*ne", Ok("Going to `where-the.Sun!don't_Sh*ne`.")),
	("go - to someplace", Err(())),
	("yes", Ok("maybe yup")),
	("no", Ok("maybe yup")),
	("YES", Ok("maybe yup")),
	("i 48", Ok("i Some(48) | f None")),
	("f 18", Ok("i None | f Some(18)")),
	("identify banana", Ok("definitely a banana")),
	("identify BaNaNa", Ok("definitely a banana")),
	("identify BaNaNa", Ok("definitely a banana")),
	("identify crowbar", Ok("giant metal banana")),
	("identify human  finger ", Ok("tiny crunchy banana")),
	("identify na na na na na na", Ok("Batman!")),
	("identify バナナ", Ok("probably not a banana")),
	("please", Ok("level 0 pleading achieved!")),
	("pretty please", Ok("level 1 pleading achieved!")),
	("pretty pretty please", Ok("level 2 pleading achieved!")),
	("the last word is", Ok("well, it is")),
	("the last word is optional", Ok("well, it is")),
];

#[test]
fn test_scan() {
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
		"a b c" => "a b c".into_string(),
		"n" n:int => format!("n {}", n),
		"s" s:&str => format!("s {}", s),
		"s(" s:&str ")" => format!("s({})", s),

		//
		// From old test example program:
		//

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

		// Match "vecs" followed by a vector of vectors.
		("vecs"|"vechs"|"vetches") [ "[" [vss:f64],* "]" ],* => format!("vss: {}", vss),

		// Match a vector of either an int or a word.
		"ints and words:" [is:int | ss:&str]* => format!("is: {}, ss: {}", is, ss),

		// Match a list of words separated by "and" or a comma.
		"words:" [words:&str]("and"|", and"|",")+ => format!("words: {}", words),

		// Match a list of numbers separated by words.
		"int words:" [is:int](ss:&str)* => format!("is: {}, ss: {}", is, ss),

		// Match "go" followed by a single word.
		"go" exit:&str => format!("Leaving by the {} exit.", exit),

		// Match "go" followed by an *identifier*.
		#[tokenizer="IdentsAndInts"]
		"goto" exit:&str => format!("Sending attack raptor to {}.", exit),

		// Match "go-to" followed by a whitespace-delimited token.
		#[tokenizer="SpaceDelimited"]
		"go-to" exit:&str => format!("Going to `{}`.", exit),

		// Match "yes" or "no".
		"yes" | "no" => "maybe yup".into_string(),

		// Match an integer or a float.
		"i" i:int | "f" f:f64 => format!("i {} | f {}", i, f),

		// Match "identify", with the remainder of the input captured by "tail".
		"identify", ..tail => identify_banana(tail).into_string(),

		// Count the degree of pleading.
		["pretty" ps:()]* "please" => format!("level {} pleading achieved!", ps.len()),

		// Self-referential pattern.
		"the last word is" "optional"? => "well, it is".into_string(),
	}
}

fn identify_banana<S: Str>(s: S) -> &'static str {
	(scan! {
		s,
		"banana" => "definitely a banana",
		"crowbar" => "giant metal banana",
		"human finger" => "tiny crunchy banana",
		["na"]{6} => "Batman!",
		.._ => "probably not a banana"
	}).unwrap()
}

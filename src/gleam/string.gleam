/// A built-in representation for efficient string manipulation. String literals
/// are enclosed in `"double quotes"`.
///
import gleam/iodata
import gleam/list
import gleam/order




/// ## Basics




/// Determine if a string is empty.
///
/// ## Examples
/// ```
/// > is_empty("") == True
/// > isEmpty("the world") == False
/// ```
///
pub fn is_empty(str: String) -> Bool {
  case str {
    "" -> True
    _ -> False
  }
}


/// Get the length of a 
///
/// > length("innumerable") == 11
/// > length("") == 0
///
pub external fn length(String) -> Int = "string" "length"


/// Repeat a string `n` times.
///
/// > repeat(3, "ha") == "hahaha"
///
// pub fn repeat(string: String, times: Int) -> String {}


/// Reverse a string.
///
/// > reverse("stressed") == "desserts"
///
pub fn reverse(string: String) -> String {
  string
  |> iodata.new
  |> iodata.reverse
  |> iodata.to_string
}


/// Replace all occurrences of some substring.
///
/// > replace("Json.Decode.succeed", all: ".", with: "-") == "Json-Decode-succeed"
/// > replace("a,b,c,d,e", all: ",", with: "/")           == "a/b/c/d/e"
pub fn replace(
  in string: String,
  all pattern: String,
  with substitute: String,
) -> String {
  string
  |> iodata.new
  |> iodata.replace(_, all: pattern, with: substitute)
  |> iodata.to_string
}


/// Convert a string to all lower case. Useful for case-insensitive comparisons.
///
/// > lowercase("X-FILES") == "x-files"
/// 
pub external fn lowercase(String) -> String = "string" "lowercase"


/// Convert a string to all upper case. Useful for case-insensitive comparisons
/// and VIRTUAL YELLING.
///
/// > uppercase("skinner") == "SKINNER"
/// 
pub external fn uppercase(String) -> String = "string" "uppercase"


/// Determines the order of the two strings.
///
/// > compare("Billy", "Anthony") == order.Gt
/// > compare("Anthony", "Billy") == order.Lt
/// > compare("Anthony", "Anthony") == order.Eq
///
pub external fn compare(String, String) -> order.Order =
  "gleam_stdlib" "compare_strings"





/// ## Get Substrings




/// Take a substring given a start and end index. Negative indexes
/// are taken starting from the *end* of the list.
///
/// > slice(start: 7, end: 9, "snakes on a plane!") == "on"
/// > slice(start: 0, end: 6, "snakes on a plane!") == "snakes"
/// > slice(start: 0, end: -7, "snakes on a plane!") == "snakes on a"
/// > slice(start: -6, end: -1, "snakes on a plane!") == "plane"
/// 
// pub fn slice(string: String, start: Int, end: Int) -> String {}


/// Take *n* characters from the left side of a 
///
/// > left(num: 2, string: "Mulder") == "Mu"
/// 
// pub fn left(from string: String, num n: Int) -> String {}


/// Take *n* characters from the right side of a 
///
/// > right("Scully", 2) == "ly"
/// 
// pub fn right(from string: String, num_characters: Int) -> String {}


/// Drop *n* characters from the left side of a 
///
/// > dropLeft(from: "The Lone Gunmen", num_characters: 2) == "e Lone Gunmen"
/// 
// pub fn drop_left(from string: String, num_characters: Int) -> String {}


/// Drop *n* characters from the right side of a 
///
/// > dropRight("Cigarette Smoking Man", 2) == "Cigarette Smoking M"
/// 
// pub fn drop_right(from string: String, num_characters: Int) -> String {}




/// ## Check for Substrings




/// See if the second string contains the first one.
///
/// > contains("theory", this: "the") == True
/// > contains("theory", this: "hat") == False
/// > contains("theory", this: "THE") == False
///
// pub fn contains(this: String, in: String) -> String {}


/// See if the second string starts with the first one.
///
/// > startsWith("theory", this: "the") == True
/// > startsWith("theory", this: "ory") == False
///
// pub fn starts_with(this: String, in: String) -> String {}


/// See if the second string ends with the first one.
///
/// > endsWith("theory", this: "the") == False
/// > endsWith("theory", this: "ory") == True
/// 
// pub fn ends_with(this: String, in: String) -> String {}




/// ## Building and Splitting




/// Split a string using a given separator.
///
/// > split("cat,dog,cow", on: ",")        == ["cat","dog","cow"]
/// > split("home/evan/Desktop/", on: "/") == ["home","evan","Desktop", ""]
/// 
pub fn split(string x: String, on pattern: String) -> List(String) {
  x
  |> iodata.new
  |> iodata.split(_, on: pattern)
  |> list.map(_, with: iodata.to_string)
}


/// Append two strings.
///
/// > append(to: "butter", suffix: "fly") == "butterfly"
///
pub fn append(to first: String, suffix second: String) -> String {
  first
  |> iodata.new
  |> iodata.append(_, second)
  |> iodata.to_string
}


/// Concatenate many strings into one.
///
/// > concat(["never","the","less"]) == "nevertheless"
///
pub fn concat(strings: List(String)) -> String {
  strings
  |> iodata.from_strings
  |> iodata.to_string
}


/// Put many strings together with a given separator.
///
/// > join(["H","w","ii","n"], with: "a")        == "Hawaiian"
/// > join(["cat","dog","cow"], with: " ")       == "cat dog cow"
/// > join(["home","evan","Desktop"], with: "/") == "home/evan/Desktop"
///
pub fn join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(_, with: separator)
  |> iodata.from_strings
  |> iodata.to_string
}



/// Break a string into words, splitting on chunks of whitespace.
///
/// > words("How are \t you? \n Good?") == ["How","are","you?","Good?"]
/// 
// pub fn words(string: String) -> List(String) {}


/// Break a string into lines, splitting on newlines.
///
/// > lines("How are you?\nGood?") == ["How are you?", "Good?"]
/// 
// pub fn lines(string: String): List(String) {}


/// Get all of the indexes for a substring in another 
///
/// > indexes(of: "i", in:"Mississippi")   == [1,4,7,10]
/// > indexes(of: "ss", in: "Mississippi")  == [2,5]
/// > indexes(of: "needle", in: "haystack") == []
///
// pub fn indexes(of: String, in: String) -> String {}




/// ## Formatting




/// Pad a string on both sides until it has a given length.
///
/// > pad("1", to: 5, with: ' ')   == "  1  "
/// > pad("11", to: 5, with: ' ')  == "  11 "
/// > pad("121", to: 5, with: ' ') == " 121 "
///
// pub fn pad(string: String, to size: Int, with: String) -> String {}


/// Pad a string on the left until it has a given length.
///
/// > padLeft("1", to: 5, with: '.')   == "....1"
/// > padLeft("11", to: 5, with: '.')  == "...11"
/// > padLeft("121", to: 5, with: '.') == "..121"
///
// pub fn pad_left(string: String, to size: Int, with: String) {}


/// Pad a string on the right until it has a given length.
///
/// > padRight("1", to: 5, with: '.')   == "1...."
/// > padRight("11", to: 5, with: '.')  == "11..."
/// > padRight("121", to: 5, with: '.') == "121.."
///
// pub fn pad_right(string: String, to size: Int, with: String) {}


/// Get rid of whitespace on both sides of a 
///
/// > trim("  hats  \n") == "hats"
///
// pub fn trim(string: String) -> String {}


/// Get rid of whitespace on the left of a 
///
/// > trimLeft("  hats  \n") == "hats  \n"
///
// pub fn trim_left(string: String) -> String {}


/// Get rid of whitespace on the right of a 
///
/// > trimRight("  hats  \n") == "  hats"
///
// pub fn trim_right(string: String) -> String {}





/// ## List Conversions




// These functions convert to and from char, which currently
// does not exist as a type in Gleam.

// /// Convert a string to a list of characters.
// ///
// /// > to_list("abc") == ['a','b','c']
// ///
// pub fn to_list(string: String) -> List(String) {}

// /// Convert a list of characters into a  Can be useful if you
// /// want to create a string primarily by consing, perhaps for decoding
// /// something.
// ///
// /// > from_list(['a','b','c']) == "abc"
// ///
// // pub fn from_list(strings: List(String)) -> String {}

// /// Add a character to the beginning of a string.
// ///
// /// > cons('T', onto: "he truth is out there") == "The truth is out there"
// // pub fn cons(char: Char, onto string: String) -> String {}


/// Split a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
///
/// > uncons("abc") == Ok(('a',"bc"))
/// > uncons("")    == Error(Nil)
///
// pub fn uncons(string: String) -> Result(tuple(String, String), Nil) {}




/// ## Higher-Order Functions




/// Transform every character in a string
///
/// > map("a/b/c", with: replace(all: "/", with: ".")) == "a.b.c"
///
// pub fn map(string: String, with: fn(String) -> String) -> String {}


/// Keep only the characters that pass the test.
///
/// > filter("R2-D2", where: isDigit) == "22"
/// 
// pub fn filter(string: String, where: fn(String) -> Bool) -> String {}


/// Reduce a string from the left.
///
/// > foldl("time", into: "", with: append) == "emit"
///
// pub fn foldl(string: String, into accumulator: b, with: fn(String, b) -> b) -> b {}


/// Reduce a string from the right.
///
/// > foldr("time", into: "", with: append)  == "time"
///
// pub fn foldr(string: String, into accumulator: b, with: fn(String, b) -> b) -> b {}


/// Determine whether *any* characters pass the test.
///
/// > any("90210", that: isDigit) == True
/// > any("R2-D2", that: isDigit) == True
/// > any("heart", that: isDigit) == False
///
// pub fn any(string: String, that predicate: fn(String) -> Bool) -> Bool {}


/// Determine whether *all* characters pass the test.
///
/// > all("90210", that: isDigit) == True
/// > all("R2-D2", that: isDigit) == False
/// > all("heart", that: isDigit) == False
///
// pub fn all(string: String, that predicate: fn(String) -> Bool) -> Bool {}

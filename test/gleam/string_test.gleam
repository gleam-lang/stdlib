import gleam/string
import gleam/should
import gleam/order
import gleam/result.{Option}

pub fn length_test() {
  string.length("ß↑e̊")
  |> should.equal(_, 3)

  string.length("Gleam")
  |> should.equal(_, 5)

  string.length("")
  |> should.equal(_, 0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> should.equal(_, "gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> should.equal(_, "GLEAM")
}

pub fn reverse_test() {
  string.reverse("Gleam")
  |> should.equal(_, "maelG")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(_, ",")
  |> should.equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(_, ", ")
  |> should.equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(_, ",", "++")
  |> should.equal(_, "Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(_, " Me")
  |> should.equal(_, "Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> should.equal(_, order.Eq)

  string.compare("a", "")
  |> should.equal(_, order.Gt)

  string.compare("a", "A")
  |> should.equal(_, order.Gt)

  string.compare("A", "B")
  |> should.equal(_, order.Lt)

  string.compare("t", "ABC")
  |> should.equal(_, order.Gt)
}

pub fn contains_test() {
  "gleam"
  |> string.contains(_, "ea")
  |> should.equal(_, True)

  "gleam"
  |> string.contains(_, "x")
  |> should.equal(_, False)

  string.contains(does: "bellwether", contain: "bell")
  |> should.equal(_, True)
}

pub fn concat_test() {
  [
  "Hello", ", ", "world!",
  ]
  |> string.concat
  |> should.equal(_, "Hello, world!")
}

pub fn repeat_test() {
  "hi"
  |> string.repeat(_, times: 3)
  |> should.equal(_, "hihihi")

  "hi"
  |> string.repeat(_, 0)
  |> should.equal(_, "")

  "hi"
  |> string.repeat(_, -1)
  |> should.equal(_, "")
}

pub fn join_test() {
  [
  "Hello", "world!",
  ]
  |> string.join(_, with: ", ")
  |> should.equal(_, "Hello, world!")

  [
  "Hello", "world!",
  ]
  |> string.join(_, with: "-")
  |> should.equal(_, "Hello-world!")
}


pub fn slice_test() {
  let unicode = "Hello 👨‍👩‍👧‍👧, I 愛 you."
    // Named params
  string.slice(unicode, start: 0, length: 5)
  |> should.equal(_, "Hello")
    // Slicing across multibyte grapheme clusters
  unicode
  |> string.slice(_, 6, 4)
  |> should.equal(_, "👨‍👩‍👧‍👧, I")
    // Length extends past end of string
  unicode
  |> string.slice(_, 9, 1000)
  |> should.equal(_, "I 愛 you.")
    // Selection extends past end of string
  unicode
  |> string.slice(_, 1000, 1000)
  |> should.equal(_, "")
    // Negative length
  unicode
  |> string.slice(_, 5, -1)
  |> should.equal(_, "")
    // Negative index
  unicode
  |> string.slice(_, -5, 1)
  |> should.equal(_, "")
}

pub fn drop_left_test() {
  let unicode = "Hello 👨‍👩‍👧‍👧!"
    // Named paramaters
  string.drop_left(from: unicode, up_to: 6)
  |> should.equal(_, "👨‍👩‍👧‍👧!")
    // Drop across multibyte grapheme cluster
  unicode
  |> string.drop_left(_, 7)
  |> should.equal(_, "!")
    // Drop past end of sting
  unicode
  |> string.drop_left(_, 1000)
  |> should.equal(_, "")
    // Drop negative number
  unicode
  |> string.drop_left(_, -1)
  |> should.equal(_, unicode)
}

pub fn drop_right_test() {
  let unicode = "Hello 👨‍👩‍👧‍👧!"
    // Named paramaters
  string.drop_right(from: unicode, drop: 6)
  |> should.equal(_, "He")
    // Drop across multibyte grapheme cluster
  unicode
  |> string.drop_right(_, 7)
  |> should.equal(_, "H")
    // Drop past end of sting
  unicode
  |> string.drop_right(_, 1000)
  |> should.equal(_, "")
    // Drop negative number
  unicode
  |> string.drop_right(_, -1)
  |> should.equal(_, unicode)
}

pub fn starts_with_test() {
  let unicode = "Hello 👨‍👩‍👧‍👧, I 愛 you."
    // Named paramaters
  string.starts_with(does: unicode, start_with: "Hello ")
  |> should.equal(_, True)
    // Across grapheme clusters
  unicode
  |> string.starts_with(_, "Hello 👨‍👩‍👧‍👧,")
  |> should.equal(_, True)
    // Inverse not true
  "Hello"
  |> string.starts_with(_, unicode)
  |> should.equal(_, False)
    // Empty string
  ""
  |> string.starts_with(_, "")
  |> should.equal(_, True)
}

pub fn ends_with_test() {
  let unicode = "Hello 👨‍👩‍👧‍👧, I 愛 you."
    // Named paramaters
  string.ends_with(does: unicode, end_with: "you.")
  |> should.equal(_, True)
    // Across grapheme clusters
  unicode
  |> string.ends_with(_, "I 愛 you.")
  |> should.equal(_, True)
    // Inverse not true
  "you."
  |> string.ends_with(_, unicode)
  |> should.equal(_, False)
    // Empty string
  ""
  |> string.ends_with(_, "")
  |> should.equal(_, True)
}

pub fn pad_left_test() {
    // Named paramaters
  string.pad_left(pad: "愛", to_length: 10, with: "*")
  |> should.equal(_, "*********愛")
    // Multi char filler
  "愛"
  |> string.pad_left(_, 10, "abcd")
  |> should.equal(_, "abcdabcda愛")
    // Multi grapheme filler
  "愛"
  |> string.pad_left(_, 5, "👨‍👩‍👧‍👧🌵")
  |> should.equal(_, "👨‍👩‍👧‍👧🌵👨‍👩‍👧‍👧🌵愛")
    // Negative to_length
  "1234"
  |> string.pad_left(_, -1, "x")
  |> should.equal(_, "1234")
    // to_length positive but shorter than input length
  "1234"
  |> string.pad_left(_, 1, "x")
  |> should.equal(_, "1234")
    // empty input
  ""
  |> string.pad_left(_, 5, "-")
  |> should.equal(_, "-----")
}

pub fn pad_right_test() {
    // Named paramaters
  string.pad_right(pad: "愛", to_length: 10, with: "*")
  |> should.equal(_, "愛*********")
    // Multi char filler
  "愛"
  |> string.pad_right(_, 10, "abcd")
  |> should.equal(_, "愛abcdabcda")
    // Multi grapheme filler
  "愛"
  |> string.pad_right(_, 5, "👨‍👩‍👧‍👧🌵")
  |> should.equal(_, "愛👨‍👩‍👧‍👧🌵👨‍👩‍👧‍👧🌵")
    // Negative to_length
  "1234"
  |> string.pad_right(_, -1, "x")
  |> should.equal(_, "1234")
    // to_length positive but shorter than input length
  "1234"
  |> string.pad_right(_, 1, "x")
  |> should.equal(_, "1234")
    // empty input
  ""
  |> string.pad_right(_, 5, "-")
  |> should.equal(_, "-----")
}


pub fn trim_test(){
    // Only testing the most common as this falls directly through to erlang.
  "\f\v\t\s\n\r  愛  \f\v\t\s\n\r"
  |> string.trim
  |> should.equal(_, "愛")
}

pub fn trim_left_test(){
  "\f\v\t\s\n\r  愛  \f\v\t\s\n\r"
  |> string.trim_left
  |> should.equal(_, "愛  \f\v\t\s\n\r")
}

pub fn trim_right_test(){
  "\f\v\t\s\n\r  愛  \f\v\t\s\n\r"
  |> string.trim_right
  |> should.equal(_, "\f\v\t\s\n\r  愛")
}

pub fn to_graphemes_test(){
    // basic latin
  "abcd"
  |> string.to_graphemes
  |> should.equal(_, ["a", "b", "c", "d"])
    // Unicode with glapheme clusters
  "--👨‍👩‍👧‍👧--🌵--"
  |> string.to_graphemes
  |> should.equal(_, ["-","-","👨‍👩‍👧‍👧", "-", "-", "🌵", "-", "-"])
    // Empty string
  ""
  |> string.to_graphemes
  |> should.equal(_, [])
}

pub fn next_grapheme_test(){
    // basic
  "abc"
  |> string.next_grapheme
  |> should.equal(_, Ok( tuple("a", "bc")))
    // unicode
  "👨‍👩‍👧‍👧-🌵"
  |> string.next_grapheme
  |> should.equal(_, Ok( tuple("👨‍👩‍👧‍👧", "-🌵")))
    // empty string
  ""
  |> string.next_grapheme
  |> should.equal(_, Error(Nil))
}

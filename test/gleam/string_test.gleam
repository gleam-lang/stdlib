import gleam/option.{None, Some}
import gleam/order
import gleam/should
import gleam/string

pub fn length_test() {
  string.length("ß↑e̊")
  |> should.equal(3)

  string.length("Gleam")
  |> should.equal(5)

  string.length("")
  |> should.equal(0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> should.equal("gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> should.equal("GLEAM")
}

pub fn reverse_test() {
  string.reverse("Gleam")
  |> should.equal("maelG")
}

pub fn unicode_reverse_test() {
  string.reverse("👍 OK")
  |> should.equal("KO 👍")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(",")
  |> should.equal(["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(", ")
  |> should.equal(["Gleam", "Erlang,Elixir"])
}

pub fn split_once_test() {
  "Gleam,Erlang,Elixir"
  |> string.split_once(",")
  |> should.equal(Ok(#("Gleam", "Erlang,Elixir")))

  "Gleam"
  |> string.split_once(",")
  |> should.equal(Error(Nil))

  ""
  |> string.split_once(",")
  |> should.equal(Error(Nil))
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(",", "++")
  |> should.equal("Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(" Me")
  |> should.equal("Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> should.equal(order.Eq)

  string.compare("a", "")
  |> should.equal(order.Gt)

  string.compare("a", "A")
  |> should.equal(order.Gt)

  string.compare("A", "B")
  |> should.equal(order.Lt)

  string.compare("t", "ABC")
  |> should.equal(order.Gt)
}

pub fn contains_test() {
  "gleam"
  |> string.contains("ea")
  |> should.equal(True)

  "gleam"
  |> string.contains("x")
  |> should.equal(False)

  string.contains(does: "bellwether", contain: "bell")
  |> should.equal(True)
}

pub fn concat_test() {
  ["Hello", ", ", "world!"]
  |> string.concat
  |> should.equal("Hello, world!")
}

pub fn repeat_test() {
  "hi"
  |> string.repeat(times: 3)
  |> should.equal("hihihi")

  "hi"
  |> string.repeat(0)
  |> should.equal("")

  "hi"
  |> string.repeat(-1)
  |> should.equal("")
}

pub fn join_test() {
  ["Hello", "world!"]
  |> string.join(with: ", ")
  |> should.equal("Hello, world!")

  ["Hello", "world!"]
  |> string.join(with: "-")
  |> should.equal("Hello-world!")
}

pub fn trim_test() {
  "  hats  \n"
  |> string.trim()
  |> should.equal("hats")
}

pub fn trim_left_test() {
  "  hats  \n"
  |> string.trim_left()
  |> should.equal("hats  \n")
}

pub fn trim_right_test() {
  "  hats  \n"
  |> string.trim_right()
  |> should.equal("  hats")
}

pub fn starts_with_test() {
  "theory"
  |> string.starts_with("")
  |> should.equal(True)

  "theory"
  |> string.starts_with("the")
  |> should.equal(True)

  "theory"
  |> string.starts_with("ory")
  |> should.equal(False)

  "theory"
  |> string.starts_with("theory2")
  |> should.equal(False)
}

pub fn ends_with_test() {
  "theory"
  |> string.ends_with("")
  |> should.equal(True)

  "theory"
  |> string.ends_with("ory")
  |> should.equal(True)

  "theory"
  |> string.ends_with("the")
  |> should.equal(False)

  "theory"
  |> string.ends_with("theory2")
  |> should.equal(False)
}

pub fn slice_test() {
  "gleam"
  |> string.slice(at_index: 1, length: 2)
  |> should.equal("le")

  "gleam"
  |> string.slice(at_index: 1, length: 10)
  |> should.equal("leam")

  "gleam"
  |> string.slice(at_index: 10, length: 3)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: -2, length: 2)
  |> should.equal("am")

  "gleam"
  |> string.slice(at_index: -12, length: 2)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: 2, length: -3)
  |> should.equal("")
}

pub fn crop_test() {
  "gleam"
  |> string.crop("gl")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("le")
  |> should.equal("leam")

  string.crop(from: "gleam", before: "ea")
  |> should.equal("eam")

  "gleam"
  |> string.crop("")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("!")
  |> should.equal("gleam")
}

pub fn drop_left_test() {
  "gleam"
  |> string.drop_left(up_to: 2)
  |> should.equal("eam")

  "gleam"
  |> string.drop_left(up_to: 6)
  |> should.equal("")

  "gleam"
  |> string.drop_left(up_to: -2)
  |> should.equal("gleam")
}

pub fn drop_right_test() {
  "gleam"
  |> string.drop_right(up_to: 2)
  |> should.equal("gle")

  "gleam"
  |> string.drop_right(up_to: 5)
  |> should.equal("")

  "gleam"
  |> string.drop_right(up_to: -2)
  |> should.equal("gleam")
}

pub fn pad_left_test() {
  "121"
  |> string.pad_left(to: 5, with: ".")
  |> should.equal("..121")

  "121"
  |> string.pad_left(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_left(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_left(to: 4, with: "XY")
  |> should.equal("X121")

  "121"
  |> string.pad_left(to: 5, with: "XY")
  |> should.equal("XY121")

  "121"
  |> string.pad_left(to: 6, with: "XY")
  |> should.equal("XYX121")
}

pub fn pad_right_test() {
  "121"
  |> string.pad_right(to: 5, with: ".")
  |> should.equal("121..")

  "121"
  |> string.pad_right(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_right(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_right(to: 4, with: "XY")
  |> should.equal("121X")

  "121"
  |> string.pad_right(to: 5, with: "XY")
  |> should.equal("121XY")

  "121"
  |> string.pad_right(to: 6, with: "XY")
  |> should.equal("121XYX")
}

pub fn pop_grapheme_test() {
  "gleam"
  |> string.pop_grapheme()
  |> should.equal(Ok(#("g", "leam")))

  "g"
  |> string.pop_grapheme()
  |> should.equal(Ok(#("g", "")))

  ""
  |> string.pop_grapheme()
  |> should.equal(Error(Nil))
}

pub fn to_graphemes_test() {
  "abc"
  |> string.to_graphemes()
  |> should.equal(["a", "b", "c"])

  "a"
  |> string.to_graphemes()
  |> should.equal(["a"])

  ""
  |> string.to_graphemes()
  |> should.equal([])
}

pub fn utf_codepoint_test() {
  string.utf_codepoint(1114444)
  |> should.be_error

  string.utf_codepoint(65534)
  |> should.be_error

  string.utf_codepoint(55296)
  |> should.be_error
}

pub fn bit_string_utf_codepoint_test() {
  assert Ok(snake) = string.utf_codepoint(128013)
  should.equal(<<snake:utf8_codepoint>>, <<"🐍":utf8>>)
}

pub fn to_option_test() {
  ""
  |> string.to_option
  |> should.equal(None)

  "ok"
  |> string.to_option
  |> should.equal(Some("ok"))
}

pub fn first_test() {
  ""
  |> string.first
  |> should.be_error

  "gleam"
  |> string.first
  |> should.equal(Ok("g"))

  "⭐️ Gleam"
  |> string.first
  |> should.equal(Ok("⭐️"))

  "a"
  |> string.first
  |> should.equal(Ok("a"))
}

pub fn last_test() {
  ""
  |> string.last
  |> should.be_error

  "gleam"
  |> string.last
  |> should.equal(Ok("m"))

  "gleam "
  |> string.last
  |> should.equal(Ok(" "))

  "եոգլի"
  |> string.last
  |> should.equal(Ok("ի"))

  "a"
  |> string.last
  |> should.equal(Ok("a"))
}

pub fn capitalize_test() {
  ""
  |> string.capitalize
  |> should.equal("")

  "gleam"
  |> string.capitalize
  |> should.equal("Gleam")

  "GLEAM"
  |> string.capitalize
  |> should.equal("Gleam")

  "g l e a m"
  |> string.capitalize
  |> should.equal("G l e a m")

  "1GLEAM"
  |> string.capitalize
  |> should.equal("1gleam")

  "_gLeAm1"
  |> string.capitalize
  |> should.equal("_gleam1")

  " gLeAm1"
  |> string.capitalize
  |> should.equal(" gleam1")

  "る"
  |> string.capitalize
  |> should.equal("る")
}

type TypeForStringFromTest {
  TypeForStringFromTest
}

type TypeOfIntForStringFromTest {
  TypeOfIntForStringFromTest(Int)
}

type TypeOfIntx2ForStringFromTest {
  TypeOfIntx2ForStringFromTest(Int, Int)
}

type TypeOfListOfIntForStringFromTest {
  TypeOfListOfIntForStringFromTest(List(Int))
}

type TypeOfStringForStringFromTest {
  TypeOfStringForStringFromTest(String)
}

type TypeOfListOfStringForStringFromTest {
  TypeOfListOfStringForStringFromTest(List(String))
}

type TypeOfTupleOfListOfStringForStringFromTest {
  TypeOfTupleOfListOfStringForStringFromTest(#(List(Int), String))
}

pub fn from_test() {
  string.from(True)
  |> should.equal("True")

  string.from(False)
  |> should.equal("False")

  string.from([True, False])
  |> should.equal("[True, False]")

  string.from([False, False])
  |> should.equal("[False, False]")

  string.from([True, True])
  |> should.equal("[True, True]")

  string.from([Nil, Nil])
  |> should.equal("[Nil, Nil]")

  string.from(#(True, False))
  |> should.equal("#(True, False)")

  string.from(#(False, False))
  |> should.equal("#(False, False)")

  string.from(#(True, True))
  |> should.equal("#(True, True)")

  string.from(#(Nil, True))
  |> should.equal("#(Nil, True)")

  string.from(#(Nil, False))
  |> should.equal("#(Nil, False)")

  string.from(#(True, Nil))
  |> should.equal("#(True, Nil)")

  string.from(#(False, Nil))
  |> should.equal("#(False, Nil)")

  string.from(-1)
  |> should.equal("-1")

  string.from(0)
  |> should.equal("0")

  string.from(1)
  |> should.equal("1")

  string.from([])
  |> should.equal("[]")

  string.from([1])
  |> should.equal("[1]")

  string.from([1, 2])
  |> should.equal("[1, 2]")

  string.from([[1], [1]])
  |> should.equal("[[1], [1]]")

  string.from(-1.5)
  |> should.equal("-1.5")

  string.from(1.5)
  |> should.equal("1.5")

  string.from([1.5])
  |> should.equal("[1.5]")

  string.from("")
  |> should.equal("\"\"")

  string.from("1")
  |> should.equal("\"1\"")

  string.from("Hello Joe!")
  |> should.equal("\"Hello Joe!\"")

  string.from("💜 Gleam")
  |> should.equal("\"💜 Gleam\"")

  string.from(["1"])
  |> should.equal("[\"1\"]")

  string.from(#())
  |> should.equal("#()")

  string.from(#(1))
  |> should.equal("#(1)")

  string.from(#("1"))
  |> should.equal("#(\"1\")")

  string.from(#(1.5))
  |> should.equal("#(1.5)")

  string.from([#(1, 2, 3), #(1, 2, 3)])
  |> should.equal("[#(1, 2, 3), #(1, 2, 3)]")

  string.from(#([1, 2, 3], "🌈", #(1, "1", True)))
  |> should.equal("#([1, 2, 3], \"🌈\", #(1, \"1\", True))")

  string.from(Nil)
  |> should.equal("Nil")

  string.from(Ok(1))
  |> should.equal("Ok(1)")

  string.from(Ok(True))
  |> should.equal("Ok(True)")

  string.from(Ok(False))
  |> should.equal("Ok(False)")

  string.from(Ok(Nil))
  |> should.equal("Ok(Nil)")

  string.from(Error(2))
  |> should.equal("Error(2)")

  string.from(Error(True))
  |> should.equal("Error(True)")

  string.from(Error(False))
  |> should.equal("Error(False)")

  string.from(Error(Nil))
  |> should.equal("Error(Nil)")

  string.from(TypeForStringFromTest)
  |> should.equal("TypeForStringFromTest")

  string.from(TypeOfIntForStringFromTest(1))
  |> should.equal("TypeOfIntForStringFromTest(1)")

  string.from(TypeOfIntx2ForStringFromTest(1, 2))
  |> should.equal("TypeOfIntx2ForStringFromTest(1, 2)")

  string.from(TypeOfListOfIntForStringFromTest([1]))
  |> should.equal("TypeOfListOfIntForStringFromTest([1])")

  string.from(TypeOfStringForStringFromTest("1"))
  |> should.equal("TypeOfStringForStringFromTest(\"1\")")

  string.from(TypeOfListOfStringForStringFromTest(["1"]))
  |> should.equal("TypeOfListOfStringForStringFromTest([\"1\"])")

  string.from(TypeOfTupleOfListOfStringForStringFromTest(#([1], "a")))
  |> should.equal("TypeOfTupleOfListOfStringForStringFromTest(#([1], \"a\"))")

  string.from(TypeOfTupleOfListOfStringForStringFromTest)
  |> should.equal("//fn(a) { ... }")
}

pub fn from_fun_ok_type_test() {
  string.from(Ok)
  |> should.equal("//fn(a) { ... }")
}

pub fn from_fun_error_type_test() {
  string.from(Error)
  |> should.equal("//fn(a) { ... }")
}

fn fun_for_from_test() {
  Nil
}

pub fn from_fun_test() {
  string.from(fun_for_from_test)
  |> should.equal("//fn() { ... }")
}

fn fun_for_from_with_1_arg_test(arg) {
  arg
  Nil
}

pub fn from_with_1_arg_test() {
  string.from(fun_for_from_with_1_arg_test)
  |> should.equal("//fn(a) { ... }")
}

fn fun_for_from_with_2_args_test(a, b) {
  a
  b
  Nil
}

pub fn from_with_2_args_test() {
  string.from(fun_for_from_with_2_args_test)
  |> should.equal("//fn(a, b) { ... }")
}

pub fn from_anon_fun_test() {
  string.from(fn() { Nil })
  |> should.equal("//fn() { ... }")
}

pub fn from_anon_fun_with_1_arg_test() {
  string.from(fn(a) {
    a
    Nil
  })
  |> should.equal("//fn(a) { ... }")
}

pub fn from_anon_fun_with_2_args_test() {
  string.from(fn(a, b) {
    a
    b
    Nil
  })
  |> should.equal("//fn(a, b) { ... }")
}

pub fn from_anon_fun_with_2_other_args_test() {
  string.from(fn(x, y) {
    x
    y
    Nil
  })
  |> should.equal("//fn(a, b) { ... }")
}

const for_from_const_test_const_a = Nil

const for_from_const_test_const_b = False

const for_from_const_test_const_c = True

const for_from_const_test_const_d = 1

const for_from_const_test_const_e = "1"

pub fn from_const_test() {
  string.from(for_from_const_test_const_a)
  |> should.equal("Nil")

  string.from(for_from_const_test_const_b)
  |> should.equal("False")

  string.from(for_from_const_test_const_c)
  |> should.equal("True")

  string.from(for_from_const_test_const_d)
  |> should.equal("1")

  string.from(for_from_const_test_const_e)
  |> should.equal("\"1\"")
}

if javascript {
  /// Due to JavaScript's `Number` type `Float`s without digits return as `Int`s.
  ///
  pub fn target_from_test() {
    string.from(-1.0)
    |> should.equal("-1")

    string.from(0.0)
    |> should.equal("0")

    string.from(1.0)
    |> should.equal("1")

    string.from([1.0])
    |> should.equal("[1]")

    string.from(#(1.0))
    |> should.equal("#(1)")
  }
}

if erlang {
  /// For any other platform, the issue JavaScript has with its `Number` type does not exist.
  ///
  pub fn target_from_test() {
    string.from(-1.0)
    |> should.equal("-1.0")

    string.from(0.0)
    |> should.equal("0.0")

    string.from(1.0)
    |> should.equal("1.0")

    string.from([1.0])
    |> should.equal("[1.0]")

    string.from(#(1.0))
    |> should.equal("#(1.0)")
  }
}

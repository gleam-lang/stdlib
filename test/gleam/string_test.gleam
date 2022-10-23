import gleam/option.{None, Some}
import gleam/order
import gleam/should
import gleam/string

if erlang {
  const recursion_test_cycles = 1_000_000
}

if javascript {
  // JavaScript engines crash when exceeding a certain stack size:
  //
  // - Chrome 106 and NodeJS V16, V18, and V19 crash around 10_000+
  // - Firefox 106 crashes around 35_000+.
  // - Safari 16 crashes around 40_000+.
  const recursion_test_cycles = 40_000
}

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
  "Gleam"
  |> string.reverse
  |> should.equal("maelG")

  " Gleam"
  |> string.reverse
  |> should.equal("maelG ")

  "👍 OK"
  |> string.reverse
  |> should.equal("KO 👍")

  "👍"
  |> string.reverse
  |> should.equal("👍")

  "ÅÄÖ"
  |> string.reverse
  |> should.equal("ÖÄÅ")

  "👶🏿"
  |> string.reverse
  |> should.equal("👶🏿")

  "👶🏿"
  |> string.reverse
  |> string.reverse
  |> should.equal("👶🏿")

  "abc"
  |> string.repeat(recursion_test_cycles)
  |> string.reverse
  |> string.starts_with("cba")
  |> should.be_true
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
  |> should.be_true

  "gleam"
  |> string.contains("x")
  |> should.be_false

  string.contains(does: "bellwether", contain: "bell")
  |> should.be_true
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
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_left_test() {
  "  hats  \n"
  |> string.trim_left
  |> should.equal("hats  \n")
}

pub fn trim_right_test() {
  "  hats  \n"
  |> string.trim_right
  |> should.equal("  hats")
}

pub fn starts_with_test() {
  "theory"
  |> string.starts_with("")
  |> should.be_true

  "theory"
  |> string.starts_with("the")
  |> should.be_true

  "theory"
  |> string.starts_with("ory")
  |> should.be_false

  "theory"
  |> string.starts_with("theory2")
  |> should.be_false
}

pub fn ends_with_test() {
  "theory"
  |> string.ends_with("")
  |> should.be_true

  "theory"
  |> string.ends_with("ory")
  |> should.be_true

  "theory"
  |> string.ends_with("the")
  |> should.be_false

  "theory"
  |> string.ends_with("theory2")
  |> should.be_false
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

  "👶🏿"
  |> string.slice(at_index: 0, length: 3)
  |> should.equal("👶🏿")

  "aaa"
  |> string.repeat(recursion_test_cycles)
  |> string.slice(at_index: recursion_test_cycles / 2, length: 3)
  |> should.equal("aaa")
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
  |> string.pop_grapheme
  |> should.equal(Ok(#("g", "leam")))

  "g"
  |> string.pop_grapheme
  |> should.equal(Ok(#("g", "")))

  ""
  |> string.pop_grapheme
  |> should.equal(Error(Nil))
}

pub fn to_graphemes_test() {
  ""
  |> string.to_graphemes
  |> should.equal([])

  "\n\t\r\"\\"
  |> string.to_graphemes
  |> should.equal(["\n", "\t", "\r", "\"", "\\"])

  "a"
  |> string.to_graphemes
  |> should.equal(["a"])

  "abc"
  |> string.to_graphemes
  |> should.equal(["a", "b", "c"])

  "🌷🎁💩😜👍🏳️‍🌈"
  |> string.to_graphemes
  |> should.equal(["🌷", "🎁", "💩", "😜", "👍", "🏳️‍🌈"])

  "Ĺo͂řȩm̅"
  |> string.to_graphemes
  |> should.equal(["Ĺ", "o͂", "ř", "ȩ", "m̅"])

  "뎌쉐"
  |> string.to_graphemes
  |> should.equal(["뎌", "쉐"])

  "👨‍👩‍👦‍👦"
  |> string.to_graphemes()
  |> should.equal(["👨‍👩‍👦‍👦"])

  "ごん゙に゙ぢば"
  |> string.to_graphemes()
  |> should.equal(["ご", "ん゙", "に゙", "ぢ", "ば"])

  "パピプペポ"
  |> string.to_graphemes()
  |> should.equal(["パ", "ピ", "プ", "ペ", "ポ"])

  "Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍A̴̵̜̰͔ͫ͗͢L̠ͨͧͩ͘G̴̻͈͍͔̹̑͗̎̅͛́Ǫ̵̹̻̝̳͂̌̌͘!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞"
  |> string.to_graphemes
  |> should.equal([
    "Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍", "A̴̵̜̰͔ͫ͗͢", "L̠ͨͧͩ͘",
    "G̴̻͈͍͔̹̑͗̎̅͛́", "Ǫ̵̹̻̝̳͂̌̌͘",
    "!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞",
  ])
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

pub fn capitalise_test() {
  ""
  |> string.capitalise
  |> should.equal("")

  "gleam"
  |> string.capitalise
  |> should.equal("Gleam")

  "GLEAM"
  |> string.capitalise
  |> should.equal("Gleam")

  "g l e a m"
  |> string.capitalise
  |> should.equal("G l e a m")

  "1GLEAM"
  |> string.capitalise
  |> should.equal("1gleam")

  "_gLeAm1"
  |> string.capitalise
  |> should.equal("_gleam1")

  " gLeAm1"
  |> string.capitalise
  |> should.equal(" gleam1")

  "る"
  |> string.capitalise
  |> should.equal("る")
}

type InspectType(a, b) {
  InspectTypeZero
  InspectTypeOne(a)
  InspectTypeTwo(a, b)
}

pub fn inspect_test() {
  string.inspect(True)
  |> should.equal("True")

  string.inspect(False)
  |> should.equal("False")

  string.inspect([True, False])
  |> should.equal("[True, False]")

  string.inspect([False, False])
  |> should.equal("[False, False]")

  string.inspect([True, True])
  |> should.equal("[True, True]")

  string.inspect([Nil, Nil])
  |> should.equal("[Nil, Nil]")

  string.inspect(#(True, False))
  |> should.equal("#(True, False)")

  string.inspect(#(False, False))
  |> should.equal("#(False, False)")

  string.inspect(#(True, True))
  |> should.equal("#(True, True)")

  string.inspect(#(Nil, True))
  |> should.equal("#(Nil, True)")

  string.inspect(#(Nil, False))
  |> should.equal("#(Nil, False)")

  string.inspect(#(True, Nil))
  |> should.equal("#(True, Nil)")

  string.inspect(#(False, Nil))
  |> should.equal("#(False, Nil)")

  string.inspect(-1)
  |> should.equal("-1")

  string.inspect(0)
  |> should.equal("0")

  string.inspect(1)
  |> should.equal("1")

  string.inspect([])
  |> should.equal("[]")

  string.inspect([1])
  |> should.equal("[1]")

  string.inspect([1, 2])
  |> should.equal("[1, 2]")

  string.inspect([[1], [1]])
  |> should.equal("[[1], [1]]")

  string.inspect(-1.5)
  |> should.equal("-1.5")

  string.inspect(1.5)
  |> should.equal("1.5")

  string.inspect([1.5])
  |> should.equal("[1.5]")

  string.inspect("")
  |> should.equal("\"\"")

  string.inspect("1")
  |> should.equal("\"1\"")

  string.inspect("Hello Joe!")
  |> should.equal("\"Hello Joe!\"")

  string.inspect("Hello \"Manuel\"!")
  |> should.equal("\"Hello \\\"Manuel\\\"!\"")

  string.inspect("💜 Gleam")
  |> should.equal("\"💜 Gleam\"")

  string.inspect(["1"])
  |> should.equal("[\"1\"]")

  string.inspect(#())
  |> should.equal("#()")

  string.inspect(#(1))
  |> should.equal("#(1)")

  string.inspect(#("1"))
  |> should.equal("#(\"1\")")

  string.inspect(#(1.5))
  |> should.equal("#(1.5)")

  string.inspect([#(1, 2, 3), #(1, 2, 3)])
  |> should.equal("[#(1, 2, 3), #(1, 2, 3)]")

  string.inspect(#([1, 2, 3], "🌈", #(1, "1", True)))
  |> should.equal("#([1, 2, 3], \"🌈\", #(1, \"1\", True))")

  string.inspect(Nil)
  |> should.equal("Nil")

  string.inspect(Ok(1))
  |> should.equal("Ok(1)")

  string.inspect(Ok(True))
  |> should.equal("Ok(True)")

  string.inspect(Ok(False))
  |> should.equal("Ok(False)")

  string.inspect(Ok(Nil))
  |> should.equal("Ok(Nil)")

  string.inspect(Error(2))
  |> should.equal("Error(2)")

  string.inspect(Error(True))
  |> should.equal("Error(True)")

  string.inspect(Error(False))
  |> should.equal("Error(False)")

  string.inspect(Error(Nil))
  |> should.equal("Error(Nil)")

  string.inspect(InspectTypeZero)
  |> should.equal("InspectTypeZero")

  string.inspect(InspectTypeOne(1))
  |> should.equal("InspectTypeOne(1)")

  string.inspect(InspectTypeTwo(1, 2))
  |> should.equal("InspectTypeTwo(1, 2)")

  string.inspect(InspectTypeOne([1]))
  |> should.equal("InspectTypeOne([1])")

  string.inspect(InspectTypeOne("1"))
  |> should.equal("InspectTypeOne(\"1\")")

  string.inspect(InspectTypeOne(["1"]))
  |> should.equal("InspectTypeOne([\"1\"])")

  string.inspect(InspectTypeOne(#([1], "a")))
  |> should.equal("InspectTypeOne(#([1], \"a\"))")

  string.inspect(Ok)
  |> should.equal("//fn(a) { ... }")

  string.inspect(Error)
  |> should.equal("//fn(a) { ... }")

  string.inspect(fn() { Nil })
  |> should.equal("//fn() { ... }")

  string.inspect(fn(a) {
    a
    Nil
  })
  |> should.equal("//fn(a) { ... }")

  string.inspect(fn(a, b) {
    a
    b
    Nil
  })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(fn(x, y) {
    x
    y
    Nil
  })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(fn(foo: Int, bar: String) -> Bool {
    foo
    bar
    False
  })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(#(InspectTypeOne, InspectTypeTwo))
  |> should.equal("#(//fn(a) { ... }, //fn(a, b) { ... })")

  string.inspect(InspectTypeOne(InspectTypeZero))
  |> should.equal("InspectTypeOne(InspectTypeZero)")

  string.inspect(<<255, 2, 0>>)
  |> should.equal("<<255, 2, 0>>")
}

if javascript {
  pub fn target_inspect_test() {
    // Due to Erlang's internal representation, on Erlang this will pass, instead:
    // |> should.equal("InspectTypeZero(InspectTypeZero)")
    //
    string.inspect(#(InspectTypeZero, InspectTypeZero))
    |> should.equal("#(InspectTypeZero, InspectTypeZero)")

    // Due to JavaScript's `Number` type `Float`s without digits return as `Int`s.
    //
    string.inspect(-1.0)
    |> should.equal("-1")

    string.inspect(0.0)
    |> should.equal("0")

    string.inspect(1.0)
    |> should.equal("1")

    string.inspect([1.0])
    |> should.equal("[1]")

    string.inspect(#(1.0))
    |> should.equal("#(1)")

    // Unlike on Erlang, on JavaScript `BitString` and `String` do have a different runtime representation.
    <<"abc":utf8>>
    |> string.inspect()
    |> should.equal("<<97, 98, 99>>")
  }
}

if erlang {
  import gleam/regex

  external fn create_erlang_pid() -> String =
    "erlang" "self"

  external fn create_erlang_reference() -> String =
    "erlang" "make_ref"

  pub fn target_inspect_test() {
    // Erlang's internal representation does not allow a correct differentiation.
    // |> should.equal("#(InspectTypeZero, InspectTypeZero)")
    //
    string.inspect(#(InspectTypeZero, InspectTypeZero))
    |> should.equal("InspectTypeZero(InspectTypeZero)")

    // Unlike JavaScript, Erlang correctly differentiates between `1` and `1.0`.
    //
    string.inspect(-1.0)
    |> should.equal("-1.0")

    string.inspect(0.0)
    |> should.equal("0.0")

    string.inspect(1.0)
    |> should.equal("1.0")

    string.inspect([1.0])
    |> should.equal("[1.0]")

    string.inspect(#(1.0))
    |> should.equal("#(1.0)")

    // Looks like `//erl(<0.83.0>)`.
    assert Ok(regular_expression) =
      regex.from_string("^\\/\\/erl\\(<[0-9]+\\.[0-9]+\\.[0-9]+>\\)$")
    string.inspect(create_erlang_pid())
    |> regex.check(regular_expression, _)
    |> should.be_true

    // Looks like: `//erl(#Ref<0.1809744150.4035444737.100468>)`.
    assert Ok(regular_expression) =
      regex.from_string(
        "^\\/\\/erl\\(#Ref<[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+>\\)$",
      )
    string.inspect(create_erlang_reference())
    |> regex.check(regular_expression, _)
    |> should.be_true

    // On Erlang the runtime representation for `String` and `BitString` is indistinguishable.
    <<"abc":utf8>>
    |> string.inspect()
    |> should.equal("\"abc\"")
  }

  pub fn improper_list_inspect_test() {
    let list = improper_list_append(1, 2, 3)
    assert "//erl([1, 2 | 3])" = string.inspect(list)
  }

  // Warning: The type of this function is incorrect
  external fn improper_list_append(
    item_a,
    item_b,
    improper_tail,
  ) -> List(anything) =
    "gleam_stdlib_test_ffi" "improper_list_append"
}

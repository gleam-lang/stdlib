import gleam/option.{None, Some}
import gleam/regex.{Match, Options}
import gleam/should

pub fn from_string_test() {
  let assert Ok(re) = regex.from_string("[0-9]")

  regex.check(re, "abc123")
  |> should.be_true

  regex.check(re, "abcxyz")
  |> should.be_false

  let assert Error(_) = regex.from_string("[0-9")
}

pub fn compile_test() {
  let options = Options(case_insensitive: True, multi_line: False)
  let assert Ok(re) = regex.compile("[A-B]", options)

  regex.check(re, "abc123")
  |> should.be_true

  let options = Options(case_insensitive: False, multi_line: True)
  let assert Ok(re) = regex.compile("^[0-9]", options)

  regex.check(re, "abc\n123")
  |> should.be_true

  // On target Erlang this test will only pass if unicode and ucp flags are set
  let assert Ok(re) = regex.compile("\\s", options)
  // Em space == U+2003 == " " == used below
  regex.check(re, " ")
  |> should.be_true
}

pub fn check_test() {
  let assert Ok(re) = regex.from_string("^f.o.?")

  regex.check(re, "foo")
  |> should.be_true

  regex.check(re, "boo")
  |> should.be_false

  re
  |> regex.check(content: "foo")
  |> should.be_true

  "boo"
  |> regex.check(with: re)
  |> should.be_false

  // On target JavaScript internal `RegExp` objects are stateful when they
  // have the global or sticky flags set (e.g., /foo/g or /foo/y).
  // These following tests make sure that our implementation circumvents this.
  let assert Ok(re) = regex.from_string("^-*[0-9]+")

  regex.check(re, "1")
  |> should.be_true

  regex.check(re, "12")
  |> should.be_true

  regex.check(re, "123")
  |> should.be_true
}

pub fn split_test() {
  let assert Ok(re) = regex.from_string(" *, *")

  regex.split(re, "foo,32, 4, 9  ,0")
  |> should.equal(["foo", "32", "4", "9", "0"])
}

pub fn scan_test() {
  let assert Ok(re) = regex.from_string("Gl\\w+")

  regex.scan(re, "!Gleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  regex.scan(re, "हGleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  regex.scan(re, "𐍈Gleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  let assert Ok(re) = regex.from_string("[oi]n a(.?) (\\w+)")

  regex.scan(re, "I am on a boat in a lake.")
  |> should.equal([
    Match(content: "on a boat", submatches: [None, Some("boat")]),
    Match(content: "in a lake", submatches: [None, Some("lake")]),
  ])

  let assert Ok(re) = regex.from_string("answer (\\d+)")
  regex.scan(re, "Is the answer 42?")
  |> should.equal([Match(content: "answer 42", submatches: [Some("42")])])

  let assert Ok(re) = regex.from_string("(\\d+)")
  regex.scan(re, "hello 42")
  |> should.equal([Match(content: "42", submatches: [Some("42")])])

  regex.scan(re, "你好 42")
  |> should.equal([Match(content: "42", submatches: [Some("42")])])

  regex.scan(re, "你好 42 世界")
  |> should.equal([Match(content: "42", submatches: [Some("42")])])

  let assert Ok(re) = regex.from_string("([+|\\-])?(\\d+)(\\w+)?")
  regex.scan(re, "+36kg")
  |> should.equal([
    Match(content: "+36kg", submatches: [Some("+"), Some("36"), Some("kg")]),
  ])

  regex.scan(re, "36kg")
  |> should.equal([
    Match(content: "36kg", submatches: [None, Some("36"), Some("kg")]),
  ])

  regex.scan(re, "36")
  |> should.equal([Match(content: "36", submatches: [None, Some("36")])])

  regex.scan(re, "-36")
  |> should.equal([Match(content: "-36", submatches: [Some("-"), Some("36")])])

  regex.scan(re, "-kg")
  |> should.equal([])

  let assert Ok(re) =
    regex.from_string("var\\s*(\\w+)\\s*(int|string)?\\s*=\\s*(.*)")
  regex.scan(re, "var age int = 32")
  |> should.equal([
    Match(content: "var age int = 32", submatches: [
      Some("age"),
      Some("int"),
      Some("32"),
    ]),
  ])

  regex.scan(re, "var age = 32")
  |> should.equal([
    Match(content: "var age = 32", submatches: [Some("age"), None, Some("32")]),
  ])

  let assert Ok(re) = regex.from_string("let (\\w+) = (\\w+)")
  regex.scan(re, "let age = 32")
  |> should.equal([
    Match(content: "let age = 32", submatches: [Some("age"), Some("32")]),
  ])

  regex.scan(re, "const age = 32")
  |> should.equal([])
}

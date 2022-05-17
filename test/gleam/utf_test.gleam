import gleam/should
import gleam/utf

pub fn pop_grapheme_test() {
  "gleam"
  |> utf.pop_grapheme()
  |> should.equal(Ok(#("g", "leam")))

  "g"
  |> utf.pop_grapheme()
  |> should.equal(Ok(#("g", "")))

  ""
  |> utf.pop_grapheme()
  |> should.equal(Error(Nil))
}

pub fn to_graphemes_test() {
  "abc"
  |> utf.graphemes()
  |> should.equal(["a", "b", "c"])

  "a"
  |> utf.graphemes()
  |> should.equal(["a"])

  ""
  |> utf.graphemes()
  |> should.equal([])
}

pub fn utf_codepoint_test() {
  utf.codepoint(1114444)
  |> should.be_error

  utf.codepoint(65534)
  |> should.be_error

  utf.codepoint(55296)
  |> should.be_error

  // bit string utf codepoint test
  assert Ok(snake) = utf.codepoint(128013)
  <<snake:utf8_codepoint>>
  |> should.equal(<<"🐍":utf8>>)
}

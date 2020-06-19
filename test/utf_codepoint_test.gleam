import gleam/should
import gleam/utf_codepoint

pub fn from_int_test() {
  utf_codepoint.from_int(1114444)
  |> should.be_error

  utf_codepoint.from_int(65534)
  |> should.be_error

  utf_codepoint.from_int(55296)
  |> should.be_error

  assert Ok(snake) = utf_codepoint.from_int(128013)
  should.equal(<<snake:utf8_codepoint>>, <<"🐍":utf8>>)
}

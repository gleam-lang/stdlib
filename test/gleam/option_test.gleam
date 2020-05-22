import gleam/should
import gleam/option.{Some, None}

pub fn is_some_test() {
  option.is_some(Some(1))
  |> should.be_true

  option.is_some(None)
  |> should.be_false
}

pub fn is_none_test() {
  option.is_none(Some(1))
  |> should.be_false

  option.is_none(None)
  |> should.be_true
}

pub fn to_result_test() {
  option.to_result(Some(1), "possible_error")
  |> should.equal(Ok(1))

  option.to_result(None, "possible_error")
  |> should.equal(Error("possible_error"))
}

pub fn from_result_test() {
  option.from_result(Ok(1))
  |> should.equal(Some(1))

  option.from_result(Error("some_error"))
  |> should.equal(None)
}

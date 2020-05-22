import gleam/should
import gleam/option.{Some, None}

pub fn is_some_test() {
  option.is_some(Some(1))
  |> should.be_true

  option.is_some(None)
  |> should.be_false
}

pub fn is_none() {
  option.is_none(Some(1))
  |> should.be_false

  option.is_none(None)
  |> should.be_true
}

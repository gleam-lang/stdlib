import gleam/should
import gleam/int
import gleam/order

pub fn to_string() {
  123
  |> int.to_string
  |> should.equal(_, "123")

  -123
  |> int.to_string
  |> should.equal(_, "-123")

  123
  |> int.to_string
  |> should.equal(_, "123")
}

pub fn parse() {
  "123"
  |> int.parse
  |> should.equal(_, Ok(123))

  "-123"
  |> int.parse
  |> should.equal(_, Ok(-123))

  "0123"
  |> int.parse
  |> should.equal(_, Ok(123))

  ""
  |> int.parse
  |> should.equal(_, Error(Nil))

  "what"
  |> int.parse
  |> should.equal(_, Error(Nil))

  "1.23"
  |> int.parse
  |> should.equal(_, Error(Nil))
}

pub fn to_base_string() {
  100
  |> int.to_base_string(_, 16)
  |> should.equal(_, "64")

  -100
  |> int.to_base_string(_, 16)
  |> should.equal(_, "-64")
}

pub fn compare_test() {
  int.compare(0, 0)
  |> should.equal(_, order.Eq)

  int.compare(1, 1)
  |> should.equal(_, order.Eq)

  int.compare(0, 1)
  |> should.equal(_, order.Lt)

  int.compare(-2, -1)
  |> should.equal(_, order.Lt)

  int.compare(2, 1)
  |> should.equal(_, order.Gt)

  int.compare(-1, -2)
  |> should.equal(_, order.Gt)
}

pub fn min_test() {
  int.min(0, 0)
  |> should.equal(_, 0)

  int.min(0, 1)
  |> should.equal(_, 0)

  int.min(1, 0)
  |> should.equal(_, 0)

  int.min(-1, 2)
  |> should.equal(_, -1)

  int.min(2, -2)
  |> should.equal(_, -2)

  int.min(-1, -1)
  |> should.equal(_, -1)
}

pub fn max_test() {
  int.max(0, 0)
  |> should.equal(_, 0)

  int.max(0, 1)
  |> should.equal(_, 1)

  int.max(1, 0)
  |> should.equal(_, 1)

  int.max(-1, 2)
  |> should.equal(_, 2)

  int.max(2, -2)
  |> should.equal(_, 2)

  int.max(-1, -1)
  |> should.equal(_, -1)
}

pub fn is_even_test() {
  int.is_even(0)
  |> should.be_true

  int.is_even(2)
  |> should.be_true

  int.is_even(-2)
  |> should.be_true

  int.is_even(10006)
  |> should.be_true

  int.is_even(1)
  |> should.be_false

  int.is_even(-3)
  |> should.be_false

  int.is_even(10005)
  |> should.be_false
}

pub fn is_odd_test() {
  int.is_odd(0)
  |> should.be_false

  int.is_odd(2)
  |> should.be_false

  int.is_odd(-2)
  |> should.be_false

  int.is_odd(10006)
  |> should.be_false

  int.is_odd(1)
  |> should.be_true

  int.is_odd(-3)
  |> should.be_true

  int.is_odd(10005)
  |> should.be_true
}

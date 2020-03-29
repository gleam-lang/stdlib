import gleam/bool
import gleam/order
import gleam/should

pub fn negate_test() {
  bool.negate(True)
  |> should.be_false

  bool.negate(False)
  |> should.be_true
}

pub fn compare_test() {
  bool.compare(True, True)
  |> should.equal(_, order.Eq)

  bool.compare(True, False)
  |> should.equal(_, order.Gt)

  bool.compare(False, False)
  |> should.equal(_, order.Eq)

  bool.compare(False, True)
  |> should.equal(_, order.Lt)
}

pub fn max_test() {
  bool.max(True, True)
  |> should.equal(_, True)

  bool.max(True, False)
  |> should.equal(_, True)

  bool.max(False, False)
  |> should.equal(_, False)

  bool.max(False, True)
  |> should.equal(_, True)
}

pub fn min_test() {
  bool.min(True, True)
  |> should.equal(_, True)

  bool.min(True, False)
  |> should.equal(_, False)

  bool.min(False, False)
  |> should.equal(_, False)

  bool.min(False, True)
  |> should.equal(_, False)
}

pub fn to_int_test() {
  bool.to_int(True)
  |> should.equal(_, 1)

  bool.to_int(False)
  |> should.equal(_, 0)
}

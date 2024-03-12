import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}
import gleam/should

pub fn negate_test() {
  order.negate(Lt)
  |> should.equal(Gt)

  order.negate(Eq)
  |> should.equal(Eq)

  order.negate(Gt)
  |> should.equal(Lt)
}

pub fn to_int_test() {
  order.to_int(Lt)
  |> should.equal(-1)

  order.to_int(Eq)
  |> should.equal(0)

  order.to_int(Gt)
  |> should.equal(1)
}

pub fn compare_test() {
  order.compare(Lt, Lt)
  |> should.equal(Eq)

  order.compare(Lt, Eq)
  |> should.equal(Lt)

  order.compare(Lt, Gt)
  |> should.equal(Lt)

  order.compare(Eq, Lt)
  |> should.equal(Gt)

  order.compare(Eq, Eq)
  |> should.equal(Eq)

  order.compare(Eq, Gt)
  |> should.equal(Lt)

  order.compare(Gt, Lt)
  |> should.equal(Gt)

  order.compare(Gt, Eq)
  |> should.equal(Gt)

  order.compare(Gt, Gt)
  |> should.equal(Eq)
}

pub fn max_test() {
  order.max(Lt, Lt)
  |> should.equal(Lt)

  order.max(Lt, Eq)
  |> should.equal(Eq)

  order.max(Lt, Gt)
  |> should.equal(Gt)

  order.max(Eq, Lt)
  |> should.equal(Eq)

  order.max(Eq, Eq)
  |> should.equal(Eq)

  order.max(Eq, Gt)
  |> should.equal(Gt)

  order.max(Gt, Lt)
  |> should.equal(Gt)

  order.max(Gt, Eq)
  |> should.equal(Gt)

  order.max(Gt, Gt)
  |> should.equal(Gt)
}

pub fn min_test() {
  order.min(Lt, Lt)
  |> should.equal(Lt)

  order.min(Lt, Eq)
  |> should.equal(Lt)

  order.min(Lt, Gt)
  |> should.equal(Lt)

  order.min(Eq, Lt)
  |> should.equal(Lt)

  order.min(Eq, Eq)
  |> should.equal(Eq)

  order.min(Eq, Gt)
  |> should.equal(Eq)

  order.min(Gt, Lt)
  |> should.equal(Lt)

  order.min(Gt, Eq)
  |> should.equal(Eq)

  order.min(Gt, Gt)
  |> should.equal(Gt)
}

pub fn reverse_test() {
  [4, 5, 1]
  |> list.sort(by: order.reverse(int.compare))
  |> should.equal([5, 4, 1])
}

pub fn break_tie_test() {
  order.break_tie(in: Eq, with: Lt)
  |> should.equal(Lt)

  order.break_tie(in: Eq, with: Gt)
  |> should.equal(Gt)

  order.break_tie(in: Eq, with: Eq)
  |> should.equal(Eq)

  order.break_tie(in: Gt, with: Lt)
  |> should.equal(Gt)

  order.break_tie(in: Lt, with: Gt)
  |> should.equal(Lt)
}

pub fn lazy_break_tie_test() {
  order.lazy_break_tie(in: Eq, with: fn() { Lt })
  |> should.equal(Lt)

  order.lazy_break_tie(in: Eq, with: fn() { Gt })
  |> should.equal(Gt)

  order.lazy_break_tie(in: Eq, with: fn() { Eq })
  |> should.equal(Eq)

  order.lazy_break_tie(in: Gt, with: fn() { panic })
  |> should.equal(Gt)

  order.lazy_break_tie(in: Lt, with: fn() { panic })
  |> should.equal(Lt)
}

import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}

pub fn negate_test() {
  assert order.negate(Lt) == Gt

  assert order.negate(Eq) == Eq

  assert order.negate(Gt) == Lt
}

pub fn to_int_test() {
  assert order.to_int(Lt) == -1

  assert order.to_int(Eq) == 0

  assert order.to_int(Gt) == 1
}

pub fn compare_test() {
  assert order.compare(Lt, Lt) == Eq

  assert order.compare(Lt, Eq) == Lt

  assert order.compare(Lt, Gt) == Lt

  assert order.compare(Eq, Lt) == Gt

  assert order.compare(Eq, Eq) == Eq

  assert order.compare(Eq, Gt) == Lt

  assert order.compare(Gt, Lt) == Gt

  assert order.compare(Gt, Eq) == Gt

  assert order.compare(Gt, Gt) == Eq
}

pub fn reverse_test() {
  assert [4, 5, 1]
    |> list.sort(by: order.reverse(int.compare))
    == [5, 4, 1]
}

pub fn break_tie_test() {
  assert order.break_tie(in: Eq, with: Lt) == Lt

  assert order.break_tie(in: Eq, with: Gt) == Gt

  assert order.break_tie(in: Eq, with: Eq) == Eq

  assert order.break_tie(in: Gt, with: Lt) == Gt

  assert order.break_tie(in: Lt, with: Gt) == Lt
}

pub fn lazy_break_tie_test() {
  assert order.lazy_break_tie(in: Eq, with: fn() { Lt }) == Lt

  assert order.lazy_break_tie(in: Eq, with: fn() { Gt }) == Gt

  assert order.lazy_break_tie(in: Eq, with: fn() { Eq }) == Eq

  assert order.lazy_break_tie(in: Gt, with: fn() { panic }) == Gt

  assert order.lazy_break_tie(in: Lt, with: fn() { panic }) == Lt
}

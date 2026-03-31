import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}

pub fn negate_lt_test() {
  assert order.negate(Lt) == Gt
}

pub fn negate_eq_test() {
  assert order.negate(Eq) == Eq
}

pub fn negate_gt_test() {
  assert order.negate(Gt) == Lt
}

pub fn to_int_lt_test() {
  assert order.to_int(Lt) == -1
}

pub fn to_int_eq_test() {
  assert order.to_int(Eq) == 0
}

pub fn to_int_gt_test() {
  assert order.to_int(Gt) == 1
}

pub fn compare_lt_lt_test() {
  assert order.compare(Lt, Lt) == Eq
}

pub fn compare_lt_eq_test() {
  assert order.compare(Lt, Eq) == Lt
}

pub fn compare_lt_gt_test() {
  assert order.compare(Lt, Gt) == Lt
}

pub fn compare_eq_lt_test() {
  assert order.compare(Eq, Lt) == Gt
}

pub fn compare_eq_eq_test() {
  assert order.compare(Eq, Eq) == Eq
}

pub fn compare_eq_gt_test() {
  assert order.compare(Eq, Gt) == Lt
}

pub fn compare_gt_lt_test() {
  assert order.compare(Gt, Lt) == Gt
}

pub fn compare_gt_eq_test() {
  assert order.compare(Gt, Eq) == Gt
}

pub fn compare_gt_gt_test() {
  assert order.compare(Gt, Gt) == Eq
}

pub fn reverse_test() {
  assert list.sort([4, 5, 1], by: order.reverse(int.compare)) == [5, 4, 1]
}

pub fn break_tie_eq_lt_test() {
  assert order.break_tie(in: Eq, with: Lt) == Lt
}

pub fn break_tie_eq_gt_test() {
  assert order.break_tie(in: Eq, with: Gt) == Gt
}

pub fn break_tie_eq_eq_test() {
  assert order.break_tie(in: Eq, with: Eq) == Eq
}

pub fn break_tie_gt_lt_test() {
  assert order.break_tie(in: Gt, with: Lt) == Gt
}

pub fn break_tie_lt_gt_test() {
  assert order.break_tie(in: Lt, with: Gt) == Lt
}

pub fn lazy_break_tie_eq_lt_test() {
  assert order.lazy_break_tie(in: Eq, with: fn() { Lt }) == Lt
}

pub fn lazy_break_tie_eq_gt_test() {
  assert order.lazy_break_tie(in: Eq, with: fn() { Gt }) == Gt
}

pub fn lazy_break_tie_eq_eq_test() {
  assert order.lazy_break_tie(in: Eq, with: fn() { Eq }) == Eq
}

pub fn lazy_break_tie_gt_lazy_test() {
  assert order.lazy_break_tie(in: Gt, with: fn() { panic }) == Gt
}

pub fn lazy_break_tie_lt_lazy_test() {
  assert order.lazy_break_tie(in: Lt, with: fn() { panic }) == Lt
}

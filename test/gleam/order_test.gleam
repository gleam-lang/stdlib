import gleam/should
import gleam/order.{Lt, Eq, Gt}

pub fn reverse_test() {
  order.reverse(Lt)
  |> should.equal(_, Gt)

  order.reverse(order.Eq)
  |> should.equal(_, order.Eq)

  order.reverse(Gt)
  |> should.equal(_, Lt)
}

pub fn to_int_test() {
  order.to_int(Lt)
  |> should.equal(_, -1)

  order.to_int(order.Eq)
  |> should.equal(_, 0)

  order.to_int(Gt)
  |> should.equal(_, 1)
}

pub fn compare_test() {
  order.compare(Lt, Lt)
  |> should.equal(_, order.Eq)

  order.compare(Lt, order.Eq)
  |> should.equal(_, Lt)

  order.compare(Lt, Gt)
  |> should.equal(_, Lt)

  order.compare(order.Eq, Lt)
  |> should.equal(_, Gt)

  order.compare(order.Eq, order.Eq)
  |> should.equal(_, order.Eq)

  order.compare(order.Eq, Gt)
  |> should.equal(_, Lt)

  order.compare(Gt, Lt)
  |> should.equal(_, Gt)

  order.compare(Gt, order.Eq)
  |> should.equal(_, Gt)

  order.compare(Gt, Gt)
  |> should.equal(_, order.Eq)
}

pub fn max_test() {
  order.max(Lt, Lt)
  |> should.equal(_, Lt)

  order.max(Lt, order.Eq)
  |> should.equal(_, order.Eq)

  order.max(Lt, Gt)
  |> should.equal(_, Gt)

  order.max(order.Eq, Lt)
  |> should.equal(_, order.Eq)

  order.max(order.Eq, order.Eq)
  |> should.equal(_, order.Eq)

  order.max(order.Eq, Gt)
  |> should.equal(_, Gt)

  order.max(Gt, Lt)
  |> should.equal(_, Gt)

  order.max(Gt, order.Eq)
  |> should.equal(_, Gt)

  order.max(Gt, Gt)
  |> should.equal(_, Gt)
}

pub fn min_test() {
  order.min(Lt, Lt)
  |> should.equal(_, Lt)

  order.min(Lt, order.Eq)
  |> should.equal(_, Lt)

  order.min(Lt, Gt)
  |> should.equal(_, Lt)

  order.min(order.Eq, Lt)
  |> should.equal(_, Lt)

  order.min(order.Eq, order.Eq)
  |> should.equal(_, order.Eq)

  order.min(order.Eq, Gt)
  |> should.equal(_, order.Eq)

  order.min(Gt, Lt)
  |> should.equal(_, Lt)

  order.min(Gt, order.Eq)
  |> should.equal(_, order.Eq)

  order.min(Gt, Gt)
  |> should.equal(_, Gt)
}

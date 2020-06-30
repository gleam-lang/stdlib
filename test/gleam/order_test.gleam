import gleam/should
import gleam/order.{Eq, Gt, Lt}

pub fn reverse_test() {
  order.reverse(Lt)
  |> should.equal(Gt)

  order.reverse(order.Eq)
  |> should.equal(order.Eq)

  order.reverse(Gt)
  |> should.equal(Lt)
}

pub fn to_int_test() {
  order.to_int(Lt)
  |> should.equal(-1)

  order.to_int(order.Eq)
  |> should.equal(0)

  order.to_int(Gt)
  |> should.equal(1)
}

pub fn compare_test() {
  order.compare(Lt, Lt)
  |> should.equal(order.Eq)

  order.compare(Lt, order.Eq)
  |> should.equal(Lt)

  order.compare(Lt, Gt)
  |> should.equal(Lt)

  order.compare(order.Eq, Lt)
  |> should.equal(Gt)

  order.compare(order.Eq, order.Eq)
  |> should.equal(order.Eq)

  order.compare(order.Eq, Gt)
  |> should.equal(Lt)

  order.compare(Gt, Lt)
  |> should.equal(Gt)

  order.compare(Gt, order.Eq)
  |> should.equal(Gt)

  order.compare(Gt, Gt)
  |> should.equal(order.Eq)
}

pub fn max_test() {
  order.max(Lt, Lt)
  |> should.equal(Lt)

  order.max(Lt, order.Eq)
  |> should.equal(order.Eq)

  order.max(Lt, Gt)
  |> should.equal(Gt)

  order.max(order.Eq, Lt)
  |> should.equal(order.Eq)

  order.max(order.Eq, order.Eq)
  |> should.equal(order.Eq)

  order.max(order.Eq, Gt)
  |> should.equal(Gt)

  order.max(Gt, Lt)
  |> should.equal(Gt)

  order.max(Gt, order.Eq)
  |> should.equal(Gt)

  order.max(Gt, Gt)
  |> should.equal(Gt)
}

pub fn min_test() {
  order.min(Lt, Lt)
  |> should.equal(Lt)

  order.min(Lt, order.Eq)
  |> should.equal(Lt)

  order.min(Lt, Gt)
  |> should.equal(Lt)

  order.min(order.Eq, Lt)
  |> should.equal(Lt)

  order.min(order.Eq, order.Eq)
  |> should.equal(order.Eq)

  order.min(order.Eq, Gt)
  |> should.equal(order.Eq)

  order.min(Gt, Lt)
  |> should.equal(Lt)

  order.min(Gt, order.Eq)
  |> should.equal(order.Eq)

  order.min(Gt, Gt)
  |> should.equal(Gt)
}

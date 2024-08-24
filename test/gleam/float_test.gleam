import gleam/float
import gleam/int
import gleam/iterator
import gleam/list
import gleam/order
import gleam/result
import gleam/should

pub fn parse_test() {
  "1.23"
  |> float.parse
  |> should.equal(Ok(1.23))

  "+1.23"
  |> float.parse
  |> should.equal(Ok(1.23))

  "-1.23"
  |> float.parse
  |> should.equal(Ok(-1.23))

  "5.0"
  |> float.parse
  |> should.equal(Ok(5.0))

  "0.123456789"
  |> float.parse
  |> should.equal(Ok(0.123456789))

  "1.234e10"
  |> float.parse
  |> should.equal(Ok(1.234e10))

  "-1.234e+10"
  |> float.parse
  |> should.equal(Ok(-1.234e10))

  "1.234e-10"
  |> float.parse
  |> should.equal(Ok(1.234e-10))

  ""
  |> float.parse
  |> should.equal(Error(Nil))

  "what"
  |> float.parse
  |> should.equal(Error(Nil))

  "1"
  |> float.parse
  |> should.equal(Error(Nil))
}

pub fn to_string_test() {
  123.0
  |> float.to_string
  |> should.equal("123.0")

  -8.1
  |> float.to_string
  |> should.equal("-8.1")
}

pub fn clamp_test() {
  float.clamp(1.4, min: 1.3, max: 1.5)
  |> should.equal(1.4)

  float.clamp(1.2, min: 1.3, max: 1.5)
  |> should.equal(1.3)

  float.clamp(1.6, min: 1.3, max: 1.5)
  |> should.equal(1.5)
}

pub fn compare_test() {
  float.compare(0.0, 0.0)
  |> should.equal(order.Eq)

  float.compare(0.1, 0.1)
  |> should.equal(order.Eq)

  float.compare(0.0, 0.1)
  |> should.equal(order.Lt)

  float.compare(-2.0, -1.9)
  |> should.equal(order.Lt)

  float.compare(2.0, 1.9)
  |> should.equal(order.Gt)

  float.compare(-1.9, -2.0)
  |> should.equal(order.Gt)
}

pub fn loosely_compare_test() {
  float.loosely_compare(10.2, 10.5, tolerating: 0.0)
  |> should.equal(order.Lt)

  float.loosely_compare(10.2, with: 10.5, tolerating: 0.31)
  |> should.equal(order.Eq)

  float.loosely_compare(10.5, 10.2, 0.31)
  |> should.equal(order.Eq)

  float.loosely_compare(10.2, 10.5, 0.29)
  |> should.equal(order.Lt)

  float.loosely_compare(10.5, 10.2, 0.29)
  |> should.equal(order.Gt)

  float.loosely_compare(-10.2, -10.5, 0.31)
  |> should.equal(order.Eq)
}

pub fn loosely_equals_test() {
  float.loosely_equals(10.2, 10.5, tolerating: 0.0)
  |> should.be_false

  float.loosely_equals(10.2, with: 10.5, tolerating: 0.31)
  |> should.be_true

  float.loosely_equals(10.5, 10.2, 0.31)
  |> should.be_true

  float.loosely_equals(10.2, 10.5, 0.29)
  |> should.be_false

  float.loosely_equals(10.5, 10.2, 0.29)
  |> should.be_false

  float.loosely_equals(-10.2, -10.5, 0.31)
  |> should.be_true
}

pub fn ceiling_test() {
  8.1
  |> float.ceiling
  |> should.equal(9.0)

  -8.1
  |> float.ceiling
  |> should.equal(-8.0)

  -8.0
  |> float.ceiling
  |> should.equal(-8.0)
}

pub fn floor_test() {
  8.1
  |> float.floor
  |> should.equal(8.0)

  -8.1
  |> float.floor
  |> should.equal(-9.0)

  -8.0
  |> float.floor
  |> should.equal(-8.0)
}

pub fn round_test() {
  8.1
  |> float.round
  |> should.equal(8)

  8.4
  |> float.round
  |> should.equal(8)

  8.499
  |> float.round
  |> should.equal(8)

  8.5
  |> float.round
  |> should.equal(9)

  -8.1
  |> float.round
  |> should.equal(-8)

  -7.5
  |> float.round
  |> should.equal(-8)
}

pub fn truncate_test() {
  8.1
  |> float.truncate
  |> should.equal(8)

  8.4
  |> float.truncate
  |> should.equal(8)

  8.499
  |> float.truncate
  |> should.equal(8)

  8.5
  |> float.truncate
  |> should.equal(8)

  -8.1
  |> float.truncate
  |> should.equal(-8)

  -7.5
  |> float.truncate
  |> should.equal(-7)
}

pub fn to_precision_test() {
  float.to_precision(2.43434348473, 2)
  |> should.equal(2.43)

  float.to_precision(2.43534348473, 2)
  |> should.equal(2.44)

  float.to_precision(-2.43534348473, 2)
  |> should.equal(-2.44)

  float.to_precision(547_890.453444, -3)
  |> should.equal(548_000.0)

  float.to_precision(547_490.453444, -3)
  |> should.equal(547_000.0)

  float.to_precision(-547_490.453444, -3)
  |> should.equal(-547_000.0)

  float.to_precision(435.3224, 0)
  |> should.equal(435.0)

  float.to_precision(435.3224, -0)
  |> should.equal(435.0)
}

pub fn min_test() {
  float.min(0.0, 0.0)
  |> should.equal(0.0)

  float.min(0.3, 1.5)
  |> should.equal(0.3)

  float.min(1.0, 0.0)
  |> should.equal(0.0)

  float.min(-1.7, 2.5)
  |> should.equal(-1.7)

  float.min(-2.2, -2.2)
  |> should.equal(-2.2)

  float.min(-1.0, -1.0)
  |> should.equal(-1.0)

  float.min(-1.1, -1.0)
  |> should.equal(-1.1)
}

pub fn max_test() {
  float.max(0.0, 0.0)
  |> should.equal(0.0)

  float.max(0.3, 1.5)
  |> should.equal(1.5)

  float.max(1.0, 0.0)
  |> should.equal(1.0)

  float.max(-1.7, 2.5)
  |> should.equal(2.5)

  float.max(-2.2, -2.2)
  |> should.equal(-2.2)

  float.max(-1.0, -1.0)
  |> should.equal(-1.0)

  float.max(-1.1, -1.0)
  |> should.equal(-1.0)
}

pub fn absolute_value_test() {
  float.absolute_value(-1.0)
  |> should.equal(1.0)

  float.absolute_value(-20.6)
  |> should.equal(20.6)

  float.absolute_value(0.0)
  |> should.equal(0.0)

  float.absolute_value(1.0)
  |> should.equal(1.0)

  float.absolute_value(25.2)
  |> should.equal(25.2)
}

pub fn power_test() {
  float.power(2.0, 2.0)
  |> should.equal(Ok(4.0))

  float.power(-5.0, 3.0)
  |> should.equal(Ok(-125.0))

  float.power(10.5, 0.0)
  |> should.equal(Ok(1.0))

  float.power(16.0, 0.5)
  |> should.equal(Ok(4.0))

  float.power(2.0, -1.0)
  |> should.equal(Ok(0.5))

  float.power(2.0, -1.0)
  |> should.equal(Ok(0.5))

  // float.power(-1.0, 0.5) is equivalent to float.square_root(-1.0)
  // and should return an error as an imaginary number would otherwise
  // have to be returned
  float.power(-1.0, 0.5)
  |> should.equal(Error(Nil))

  // Check another case with a negative base and fractional exponent
  float.power(-1.5, 1.5)
  |> should.equal(Error(Nil))

  // float.power(0.0, -1.0) is equivalent to 1. /. 0 and is expected
  // to be an error
  float.power(0.0, -1.0)
  |> should.equal(Error(Nil))

  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  float.power(-2.0, -1.0)
  |> should.equal(Ok(-0.5))
}

pub fn square_root_test() {
  float.square_root(4.0)
  |> should.equal(Ok(2.0))

  float.square_root(16.0)
  |> should.equal(Ok(4.0))

  float.square_root(0.0)
  |> should.equal(Ok(0.0))

  float.square_root(-4.0)
  |> should.equal(Error(Nil))
}

pub fn negate_test() {
  float.negate(-1.0)
  |> should.equal(1.0)

  float.negate(2.0)
  |> should.equal(-2.0)

  float.negate(0.0)
  |> float.negate
  |> should.equal(0.0)
}

pub fn sum_test() {
  float.sum([])
  |> should.equal(0.0)

  float.sum([1.0, 2.2, 3.3])
  |> should.equal(6.5)
}

pub fn product_test() {
  float.product([])
  |> should.equal(1.0)

  float.product([4.0])
  |> should.equal(4.0)

  float.product([2.5, 3.2, 4.2])
  |> should.equal(33.6)
}

pub fn random_test() {
  let expected_average = 0.5
  let iterations = 10_000
  let sum =
    list.range(0, iterations)
    |> iterator.from_list()
    |> iterator.fold(from: 0.0, with: fn(accumulator, _element) {
      let i = float.random()

      { i <. 1.0 }
      |> should.be_true
      { i >=. 0.0 }
      |> should.be_true

      accumulator +. i
    })
  let average = sum /. int.to_float(iterations)

  { average <. expected_average +. 0.1 }
  |> should.be_true
  { average >. expected_average -. 0.1 }
  |> should.be_true
}

pub fn modulo_test() {
  float.modulo(13.3, by: 0.0)
  |> should.equal(Error(Nil))

  float.modulo(13.3, by: 3.3)
  |> result.unwrap(or: 0.0)
  |> float.loosely_equals(with: 0.1, tolerating: 0.001)
  |> should.be_true

  float.modulo(-13.3, by: 3.3)
  |> result.unwrap(or: 0.0)
  |> float.loosely_equals(with: 3.2, tolerating: 0.001)
  |> should.be_true

  float.modulo(13.3, by: -3.3)
  |> result.unwrap(or: 0.0)
  |> float.loosely_equals(with: -3.2, tolerating: 0.001)
  |> should.be_true

  float.modulo(-13.3, by: -3.3)
  |> result.unwrap(or: 0.0)
  |> float.loosely_equals(with: -0.1, tolerating: 0.001)
  |> should.be_true
}

pub fn divide_test() {
  float.divide(1.0, 1.0)
  |> should.equal(Ok(1.0))
  float.divide(1.0, 0.0)
  |> should.equal(Error(Nil))

  float.divide(0.0, by: 1.0)
  |> should.equal(Ok(0.0))
  float.divide(1.0, by: 0.0)
  |> should.equal(Error(Nil))
}

pub fn add_test() {
  float.add(1.0, 2.0)
  |> should.equal(3.0)

  3.0
  |> float.add(2.0)
  |> should.equal(5.0)
}

pub fn multiply_test() {
  float.multiply(2.0, 4.0)
  |> should.equal(8.0)

  3.0
  |> float.multiply(2.0)
  |> should.equal(6.0)
}

pub fn subtract_test() {
  float.subtract(3.0, 1.0)
  |> should.equal(2.0)

  3.0
  |> float.subtract(2.0)
  |> should.equal(1.0)

  3.0
  |> float.subtract(2.0, _)
  |> should.equal(-1.0)
}

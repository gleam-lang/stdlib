import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/result

pub fn parse_test() {
  assert float.parse("1.23") == Ok(1.23)

  assert float.parse("+1.23") == Ok(1.23)

  assert float.parse("-1.23") == Ok(-1.23)

  assert float.parse("5.0") == Ok(5.0)

  assert float.parse("0.123456789") == Ok(0.123456789)

  assert float.parse("1.234e10") == Ok(1.234e10)

  assert float.parse("-1.234e+10") == Ok(-1.234e10)

  assert float.parse("1.234e-10") == Ok(1.234e-10)

  assert float.parse("") == Error(Nil)

  assert float.parse("what") == Error(Nil)

  assert float.parse("1") == Error(Nil)
}

pub fn to_string_test() {
  assert float.to_string(0.0) == "0.0"

  assert float.to_string(0.0123) == "0.0123"

  assert float.to_string(-0.0123) == "-0.0123"

  assert float.to_string(12.67) == "12.67"

  assert float.to_string(-12.67) == "-12.67"

  assert float.to_string(123.0) == "123.0"

  assert float.to_string(-123.0) == "-123.0"

  assert float.to_string(3.0e26) == "3.0e26"

  assert float.to_string(-3.0e26) == "-3.0e26"

  assert float.to_string(3.0e-26) == "3.0e-26"

  assert float.to_string(-3.0e-26) == "-3.0e-26"

  assert float.to_string(456.12e78) == "4.5612e80"

  assert float.to_string(-456.12e78) == "-4.5612e80"

  assert float.to_string(456.12e-78) == "4.5612e-76"

  assert float.to_string(-456.12e-78) == "-4.5612e-76"
}

pub fn clamp_test() {
  assert float.clamp(1.4, min: 1.3, max: 1.5) == 1.4

  assert float.clamp(1.2, min: 1.3, max: 1.5) == 1.3

  assert float.clamp(1.6, min: 1.3, max: 1.5) == 1.5

  assert float.clamp(1.2, min: 1.4, max: 0.6) == 1.2
}

pub fn compare_test() {
  assert float.compare(0.0, 0.0) == order.Eq

  assert float.compare(0.1, 0.1) == order.Eq

  assert float.compare(0.0, 0.1) == order.Lt

  assert float.compare(-2.0, -1.9) == order.Lt

  assert float.compare(2.0, 1.9) == order.Gt

  assert float.compare(-1.9, -2.0) == order.Gt
}

pub fn loosely_compare_test() {
  assert float.loosely_compare(10.2, 10.5, tolerating: 0.0) == order.Lt

  assert float.loosely_compare(10.2, with: 10.5, tolerating: 0.31) == order.Eq

  assert float.loosely_compare(10.5, 10.2, 0.31) == order.Eq

  assert float.loosely_compare(10.2, 10.5, 0.29) == order.Lt

  assert float.loosely_compare(10.5, 10.2, 0.29) == order.Gt

  assert float.loosely_compare(-10.2, -10.5, 0.31) == order.Eq
}

pub fn loosely_equals_test() {
  assert !float.loosely_equals(10.2, 10.5, tolerating: 0.0)

  assert float.loosely_equals(10.2, with: 10.5, tolerating: 0.31)

  assert float.loosely_equals(10.5, 10.2, 0.31)

  assert !float.loosely_equals(10.2, 10.5, 0.29)

  assert !float.loosely_equals(10.5, 10.2, 0.29)

  assert float.loosely_equals(-10.2, -10.5, 0.31)
}

pub fn ceiling_test() {
  assert float.ceiling(8.1) == 9.0

  assert float.ceiling(-8.1) == -8.0

  assert float.ceiling(-8.0) == -8.0
}

pub fn floor_test() {
  assert float.floor(8.1) == 8.0

  assert float.floor(-8.1) == -9.0

  assert float.floor(-8.0) == -8.0
}

pub fn round_test() {
  assert float.round(8.1) == 8

  assert float.round(8.4) == 8

  assert float.round(8.499) == 8

  assert float.round(8.5) == 9

  assert float.round(-8.1) == -8

  assert float.round(-7.5) == -8
}

pub fn truncate_test() {
  assert float.truncate(8.1) == 8

  assert float.truncate(8.4) == 8

  assert float.truncate(8.499) == 8

  assert float.truncate(8.5) == 8

  assert float.truncate(-8.1) == -8

  assert float.truncate(-7.5) == -7
}

pub fn to_precision_test() {
  assert float.to_precision(2.43434348473, 2) == 2.43

  assert float.to_precision(2.43534348473, 2) == 2.44

  assert float.to_precision(-2.43534348473, 2) == -2.44

  assert float.to_precision(547_890.453444, -3) == 548_000.0

  assert float.to_precision(547_490.453444, -3) == 547_000.0

  assert float.to_precision(-547_490.453444, -3) == -547_000.0

  assert float.to_precision(435.3224, 0) == 435.0

  assert float.to_precision(435.3224, -0) == 435.0

  assert float.to_precision(184.20000000000002, 2) == 184.2

  assert float.to_precision(12_345_678_912_345_678_912_345_678.0, -19)
    == 1_234_568.0e19
}

pub fn min_test() {
  assert float.min(0.0, 0.0) == 0.0

  assert float.min(0.3, 1.5) == 0.3

  assert float.min(1.0, 0.0) == 0.0

  assert float.min(-1.7, 2.5) == -1.7

  assert float.min(-2.2, -2.2) == -2.2

  assert float.min(-1.0, -1.0) == -1.0

  assert float.min(-1.1, -1.0) == -1.1
}

pub fn max_test() {
  assert float.max(0.0, 0.0) == 0.0

  assert float.max(0.3, 1.5) == 1.5

  assert float.max(1.0, 0.0) == 1.0

  assert float.max(-1.7, 2.5) == 2.5

  assert float.max(-2.2, -2.2) == -2.2

  assert float.max(-1.0, -1.0) == -1.0

  assert float.max(-1.1, -1.0) == -1.0
}

pub fn absolute_value_test() {
  assert float.absolute_value(-1.0) == 1.0

  assert float.absolute_value(-20.6) == 20.6

  assert float.absolute_value(0.0) == 0.0

  assert float.absolute_value(1.0) == 1.0

  assert float.absolute_value(25.2) == 25.2
}

pub fn power_test() {
  assert float.power(2.0, 2.0) == Ok(4.0)

  assert float.power(-5.0, 3.0) == Ok(-125.0)

  assert float.power(10.5, 0.0) == Ok(1.0)

  assert float.power(16.0, 0.5) == Ok(4.0)

  assert float.power(2.0, -1.0) == Ok(0.5)

  assert float.power(2.0, -1.0) == Ok(0.5)

  // float.power(-1.0, 0.5) is equivalent to float.square_root(-1.0)
  // and should return an error as an imaginary number would otherwise
  // have to be returned
  assert float.power(-1.0, 0.5) == Error(Nil)

  // Check another case with a negative base and fractional exponent
  assert float.power(-1.5, 1.5) == Error(Nil)

  // float.power(0.0, -1.0) is equivalent to 1. /. 0 and is expected
  // to be an error
  assert float.power(0.0, -1.0) == Error(Nil)

  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  assert float.power(-2.0, -1.0) == Ok(-0.5)
}

pub fn square_root_test() {
  assert float.square_root(4.0) == Ok(2.0)

  assert float.square_root(16.0) == Ok(4.0)

  assert float.square_root(0.0) == Ok(0.0)

  assert float.square_root(-4.0) == Error(Nil)
}

pub fn negate_test() {
  assert float.negate(-1.0) == 1.0

  assert float.negate(2.0) == -2.0

  assert float.negate(float.negate(0.0)) == 0.0
}

pub fn sum_test() {
  assert float.sum([]) == 0.0

  assert float.sum([1.0, 2.2, 3.3]) == 6.5
}

pub fn product_test() {
  assert float.product([]) == 1.0

  assert float.product([4.0]) == 4.0

  assert float.product([2.5, 3.2, 4.2]) == 33.6
}

pub fn random_test() {
  let expected_average = 0.5
  let iterations = 10_000
  let sum =
    list.range(0, iterations)
    |> list.fold(from: 0.0, with: fn(accumulator, _element) {
      let i = float.random()

      assert { i <. 1.0 }
      assert { i >=. 0.0 }

      accumulator +. i
    })
  let average = sum /. int.to_float(iterations)

  assert { average <. expected_average +. 0.1 }
  assert { average >. expected_average -. 0.1 }
}

pub fn modulo_test() {
  assert float.modulo(13.3, by: 0.0) == Error(Nil)

  assert float.modulo(13.3, by: 3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 0.1, tolerating: 0.001)

  assert float.modulo(-13.3, by: 3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 3.2, tolerating: 0.001)

  assert float.modulo(13.3, by: -3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -3.2, tolerating: 0.001)

  assert float.modulo(-13.3, by: -3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -0.1, tolerating: 0.001)
}

pub fn divide_test() {
  assert float.divide(1.0, 1.0) == Ok(1.0)
  assert float.divide(1.0, 0.0) == Error(Nil)

  assert float.divide(0.0, by: 1.0) == Ok(0.0)
  assert float.divide(1.0, by: 0.0) == Error(Nil)
}

pub fn add_test() {
  assert float.add(1.0, 2.0) == 3.0

  assert float.add(3.0, 2.0) == 5.0
}

pub fn multiply_test() {
  assert float.multiply(2.0, 4.0) == 8.0

  assert float.multiply(3.0, 2.0) == 6.0
}

pub fn subtract_test() {
  assert float.subtract(3.0, 1.0) == 2.0

  assert float.subtract(3.0, 2.0) == 1.0

  assert float.subtract(2.0, 3.0) == -1.0
}

pub fn logarithm_test() {
  assert float.logarithm(1.0)
    |> result.unwrap(or: 1.0)
    |> float.loosely_equals(with: 0.0, tolerating: 0.001)

  assert float.logarithm(2.718281828459045)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 1.0, tolerating: 0.001)

  assert float.logarithm(10.0)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 2.302585092994046, tolerating: 0.001)

  assert float.logarithm(100.0)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 4.605170185988092, tolerating: 0.001)

  assert float.logarithm(0.5)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -0.6931471805599453, tolerating: 0.001)

  assert float.logarithm(0.1)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -2.3025850929940455, tolerating: 0.001)

  assert float.logarithm(0.0) == Error(Nil)

  assert float.logarithm(-1.0) == Error(Nil)

  assert float.logarithm(-100.0) == Error(Nil)

  assert float.logarithm(-0.1) == Error(Nil)
}

pub fn exponential_test() {
  assert float.loosely_equals(
    float.exponential(0.0),
    with: 1.0,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(1.0),
    with: 2.718281828459045,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(2.0),
    with: 7.38905609893065,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(-1.0),
    with: 0.36787944117144233,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(5.0),
    with: 148.4131591025766,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(-5.0),
    with: 0.006737946999085467,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(0.000001),
    with: 1.0000010000005,
    tolerating: 0.001,
  )

  assert float.loosely_equals(
    float.exponential(-100.0),
    with: 3.720075976020836e-44,
    tolerating: 0.001,
  )
}

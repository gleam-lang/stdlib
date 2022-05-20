import gleam/float
import gleam/int
import gleam/iterator
import gleam/list
import gleam/order
import gleam/should
import gleam/io

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
  float.compare(0., 0.)
  |> should.equal(order.Eq)

  float.compare(0.1, 0.1)
  |> should.equal(order.Eq)

  float.compare(0., 0.1)
  |> should.equal(order.Lt)

  float.compare(-2., -1.9)
  |> should.equal(order.Lt)

  float.compare(2., 1.9)
  |> should.equal(order.Gt)

  float.compare(-1.9, -2.)
  |> should.equal(order.Gt)
}

pub fn loosely_compare_test() {
  float.loosely_compare(10.2, 10.5, tolerating: 0.)
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

pub fn min_test() {
  float.min(0., 0.)
  |> should.equal(0.)

  float.min(0.3, 1.5)
  |> should.equal(0.3)

  float.min(1., 0.)
  |> should.equal(0.)

  float.min(-1.7, 2.5)
  |> should.equal(-1.7)

  float.min(-2.2, -2.2)
  |> should.equal(-2.2)

  float.min(-1., -1.)
  |> should.equal(-1.)

  float.min(-1.1, -1.)
  |> should.equal(-1.1)
}

pub fn max_test() {
  float.max(0., 0.)
  |> should.equal(0.)

  float.max(0.3, 1.5)
  |> should.equal(1.5)

  float.max(1., 0.)
  |> should.equal(1.)

  float.max(-1.7, 2.5)
  |> should.equal(2.5)

  float.max(-2.2, -2.2)
  |> should.equal(-2.2)

  float.max(-1., -1.)
  |> should.equal(-1.)

  float.max(-1.1, -1.)
  |> should.equal(-1.)
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
  |> should.equal(4.0)

  float.power(-5.0, 3.0)
  |> should.equal(-125.0)

  float.power(10.5, 0.0)
  |> should.equal(1.0)

  float.power(16.0, 0.5)
  |> should.equal(4.0)

  float.power(2.0, -1.0)
  |> should.equal(0.5)
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
  float.negate(-1.)
  |> should.equal(1.)

  float.negate(2.)
  |> should.equal(-2.)

  float.negate(0.)
  |> should.equal(0.)
}

pub fn sum_test() {
  float.sum([])
  |> should.equal(0.0)

  float.sum([1.0, 2.2, 3.3])
  |> should.equal(6.5)
}

pub fn product_test() {
  float.product([])
  |> should.equal(0.)

  float.product([4.])
  |> should.equal(4.)

  float.product([2.5, 3.2, 4.2])
  |> should.equal(33.6)
}

pub fn random_test() {
  let test_boundaries = fn(_accumulator, _element) {
    float.random(0.0, 0.0)
    |> should.equal(0.0)

    float.random(0.0, 10.0)
    |> fn(x) { x >=. 0.0 && x <. 10.0 }
    |> should.be_true

    float.random(10.0, 0.0)
    |> fn(x) { x >=. 0.0 && x <. 10.0 }
    |> should.be_true

    float.random(0.0, -10.0)
    |> fn(x) { x >=. -10.0 && x <. 0.0 }
    |> should.be_true

    float.random(-10.0, 0.0)
    |> fn(x) { x >=. -10.0 && x <. 0.0 }
    |> should.be_true

    float.random(-10.0, 10.0)
    |> fn(x) { x >=. -10.0 && x <. 10.0 }
    |> should.be_true

    float.random(10.0, -10.0)
    |> fn(x) { x >=. -10.0 && x <. 10.0 }
    |> should.be_true
  }
  list.range(0, 100)
  |> iterator.from_list()
  |> iterator.fold(Nil, test_boundaries)

  let test_mean = fn(iterations: Int, min: Float, max: Float, tolerance: Float) {
    let expected_average = float.sum([min, max]) /. 2.0
    list.range(0, iterations)
    |> iterator.from_list()
    |> iterator.fold(
      from: 0.0,
      with: fn(accumulator, _element) { accumulator +. float.random(min, max) },
    )
    |> fn(sum) { sum /. int.to_float(iterations) }
    |> float.loosely_compare(expected_average, tolerance)
    |> should.equal(order.Eq)
  }
  test_mean(100, 0.0, 0.0, 5.0)
  test_mean(1_000, 0.0, 100.0, 5.0)
  test_mean(1_000, -100.0, 100.0, 5.0)
  test_mean(1_000, -100.0, 0.0, 5.0)
  test_mean(1_000, 0.0, -100.0, 5.0)
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

pub fn math_sin_test() {
  float.sin(0.0)
  |> should.equal(0.0)

  float.sin(0.5 *. float.pi())
  |> should.equal(1.0)

  float.sin(0.5)
  |> float.loosely_compare(0.479425, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_asin_test() {
  float.asin(0.0)
  |> should.equal(Ok(0.0))

  assert Ok(result) = float.asin(0.5)
  result
  |> float.loosely_compare(0.523598, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.asin(1.1)
  |> should.equal(Error(Nil))

  float.asin(-1.1)
  |> should.equal(Error(Nil))
}

pub fn math_sinh_test() {
  float.sinh(0.0)
  |> should.equal(0.0)

  float.sinh(0.5)
  |> float.loosely_compare(0.521095, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_asinh_test() {
  float.asinh(0.0)
  |> should.equal(0.0)

  float.asinh(0.5)
  |> float.loosely_compare(0.481211, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_cos_test() {
  float.cos(0.0)
  |> should.equal(1.0)

  float.cos(float.pi())
  |> should.equal(-1.0)

  float.cos(0.5)
  |> float.loosely_compare(0.877582, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_acos_test() {
  float.acos(1.0)
  |> should.equal(Ok(0.0))

  assert Ok(result) = float.acos(0.5)
  result
  |> float.loosely_compare(1.047197, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.acos(1.1)
  |> should.equal(Error(Nil))

  float.acos(-1.1)
  |> should.equal(Error(Nil))
}

pub fn math_cosh_test() {
  float.cosh(0.0)
  |> should.equal(1.0)

  float.cosh(0.5)
  |> float.loosely_compare(1.127625, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_acosh_test() {
  float.acosh(1.0)
  |> should.equal(Ok(0.0))

  assert Ok(result) = float.acosh(5.)
  result
  |> float.loosely_compare(2.292431, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.acosh(0.0)
  |> should.equal(Error(Nil))
}

pub fn math_tan_test() {
  float.tan(0.0)
  |> should.equal(0.0)

  float.tan(0.5)
  |> float.loosely_compare(0.546302, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_atan_test() {
  float.atan(0.0)
  |> should.equal(0.0)

  float.atan(0.5)
  |> float.loosely_compare(0.463647, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_tanh_test() {
  float.tanh(0.0)
  |> should.equal(0.0)

  float.tanh(25.0)
  |> should.equal(1.0)

  float.tanh(-25.0)
  |> should.equal(-1.0)

  float.tanh(0.5)
  |> float.loosely_compare(0.462117, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_atanh_test() {
  float.atanh(0.0)
  |> should.equal(Ok(0.0))

  assert Ok(result) = float.atanh(0.5)
  result
  |> float.loosely_compare(0.549306, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.atanh(1.0)
  |> should.equal(Error(Nil))

  float.atanh(-1.0)
  |> should.equal(Error(Nil))
}

pub fn math_atan2_test() {
  float.atan2(0.0, 0.0)
  |> should.equal(0.0)

  float.atan2(0.0, 1.0)
  |> should.equal(0.0)

  // Check identity atan2(y=1.0, x=0.5) == atan(y / x) 
  // for any x > 0 and any y
  float.atan2(1.0, 0.5)
  |> should.equal(float.atan(1.0 /. 0.5))

  // Check identity atan2(y=2.0, x=-1.5) == pi + atan(y / x) 
  // for any x < 0 and y >= 0
  float.atan2(2.0, -1.5)
  |> should.equal(float.pi() +. float.atan(2.0 /. -1.5))

  // Check identity atan2(y=-2.0, x=-1.5) == atan(y / x) - pi  
  // for any x < 0 and y < 0
  float.atan2(-2.0, -1.5)
  |> should.equal(float.atan(-2.0 /. -1.5) -. float.pi())

  // Check identity atan2(y=1.5, x=0.0) == pi/2 
  // for x = 0 and any y > 0
  float.atan2(1.5, 0.0)
  |> should.equal(float.pi() /. 2.)

  // Check identity atan2(y=-1.5, x=0.0) == -pi/2 
  // for x = 0 and any y < 0
  float.atan2(-1.5, 0.0)
  |> should.equal(-1. *. float.pi() /. 2.)
}

pub fn math_exp_test() {
  float.exp(0.0)
  |> should.equal(1.0)

  float.exp(0.5)
  |> float.loosely_compare(1.648721, float.power(-10., -6.))
  |> should.equal(order.Eq)
}

pub fn math_log_test() {
  float.log(1.0)
  |> should.equal(Ok(0.0))

  float.log(float.exp(1.0))
  |> should.equal(Ok(1.0))

  assert Ok(result) = float.log(0.5)
  result
  |> float.loosely_compare(-0.693147, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.log(-1.0)
  |> should.equal(Error(Nil))
}

pub fn math_log2_test() {
  float.log2(1.0)
  |> should.equal(Ok(0.0))

  float.log2(2.0)
  |> should.equal(Ok(1.0))

  assert Ok(result) = float.log2(5.)
  result
  |> float.loosely_compare(2.321928, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.log2(-1.0)
  |> should.equal(Error(Nil))
}

pub fn math_log10_test() {
  float.log10(1.0)
  |> should.equal(Ok(0.0))

  float.log10(10.0)
  |> should.equal(Ok(1.0))

  assert Ok(result) = float.log10(50.)
  result
  |> float.loosely_compare(1.698970, float.power(-10., -6.))
  |> should.equal(order.Eq)

  float.log10(-1.0)
  |> should.equal(Error(Nil))
}

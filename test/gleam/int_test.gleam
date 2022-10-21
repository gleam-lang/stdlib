import gleam/int
import gleam/iterator
import gleam/list
import gleam/order
import gleam/should

pub fn absolute_value_test() {
  123
  |> int.absolute_value
  |> should.equal(123)

  -123
  |> int.absolute_value
  |> should.equal(123)
}

pub fn clamp_test() {
  int.clamp(40, min: 30, max: 50)
  |> should.equal(40)

  int.clamp(20, min: 30, max: 50)
  |> should.equal(30)

  int.clamp(60, min: 30, max: 50)
  |> should.equal(50)

  // If the bounds are reversed we return the min
  int.clamp(100, min: 50, max: 30)
  |> should.equal(50)
}

pub fn to_string_test() {
  123
  |> int.to_string
  |> should.equal("123")

  -123
  |> int.to_string
  |> should.equal("-123")

  123
  |> int.to_string
  |> should.equal("123")
}

pub fn parse_test() {
  "123"
  |> int.parse
  |> should.equal(Ok(123))

  "-123"
  |> int.parse
  |> should.equal(Ok(-123))

  "0123"
  |> int.parse
  |> should.equal(Ok(123))

  ""
  |> int.parse
  |> should.equal(Error(Nil))

  "what"
  |> int.parse
  |> should.equal(Error(Nil))

  "1.23"
  |> int.parse
  |> should.equal(Error(Nil))
}

pub fn to_base_string_test() {
  100
  |> int.to_base_string(16)
  |> should.equal(Ok("64"))

  -100
  |> int.to_base_string(16)
  |> should.equal(Ok("-64"))

  100
  |> int.to_base_string(1)
  |> should.equal(Error(int.InvalidBase))

  100
  |> int.to_base_string(37)
  |> should.equal(Error(int.InvalidBase))
}

pub fn to_base2_test() {
  100
  |> int.to_base2()
  |> should.equal("1100100")

  -100
  |> int.to_base2()
  |> should.equal("-1100100")
}

pub fn to_base8_test() {
  100
  |> int.to_base8()
  |> should.equal("144")

  -100
  |> int.to_base8()
  |> should.equal("-144")
}

pub fn to_base16_test() {
  100
  |> int.to_base16()
  |> should.equal("64")

  -100
  |> int.to_base16()
  |> should.equal("-64")

  43981
  |> int.to_base16()
  |> should.equal("ABCD")

  -43981
  |> int.to_base16()
  |> should.equal("-ABCD")
}

pub fn to_base36_test() {
  100
  |> int.to_base36()
  |> should.equal("2S")

  -100
  |> int.to_base36()
  |> should.equal("-2S")
}

pub fn to_float_test() {
  int.to_float(1)
  |> should.equal(1.)

  int.to_float(5)
  |> should.equal(5.)

  int.to_float(0)
  |> should.equal(0.)

  int.to_float(-5)
  |> should.equal(-5.)
}

pub fn compare_test() {
  int.compare(0, 0)
  |> should.equal(order.Eq)

  int.compare(1, 1)
  |> should.equal(order.Eq)

  int.compare(0, 1)
  |> should.equal(order.Lt)

  int.compare(-2, -1)
  |> should.equal(order.Lt)

  int.compare(2, 1)
  |> should.equal(order.Gt)

  int.compare(-1, -2)
  |> should.equal(order.Gt)
}

pub fn min_test() {
  int.min(0, 0)
  |> should.equal(0)

  int.min(0, 1)
  |> should.equal(0)

  int.min(1, 0)
  |> should.equal(0)

  int.min(-1, 2)
  |> should.equal(-1)

  int.min(2, -2)
  |> should.equal(-2)

  int.min(-1, -1)
  |> should.equal(-1)
}

pub fn max_test() {
  int.max(0, 0)
  |> should.equal(0)

  int.max(0, 1)
  |> should.equal(1)

  int.max(1, 0)
  |> should.equal(1)

  int.max(-1, 2)
  |> should.equal(2)

  int.max(2, -2)
  |> should.equal(2)

  int.max(-1, -1)
  |> should.equal(-1)
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

pub fn power_test() {
  int.power(2, 2.0)
  |> should.equal(Ok(4.0))

  int.power(-5, 3.0)
  |> should.equal(Ok(-125.0))

  int.power(10, 0.0)
  |> should.equal(Ok(1.0))

  int.power(16, 0.5)
  |> should.equal(Ok(4.0))

  int.power(2, -1.0)
  |> should.equal(Ok(0.5))

  // int.power(-1, 0.5) is equivalent to int.square_root(-1) and should
  // return an error as an imaginary number would otherwise have to be
  // returned
  int.power(-1, 0.5)
  |> should.equal(Error(Nil))

  // Check another case with a negative base and fractional exponent
  int.power(-1, 1.5)
  |> should.equal(Error(Nil))

  // float.power(0, -1) is equivalent to 1 / 0 and is expected
  // to be an error
  int.power(0, -1.0)
  |> should.equal(Error(Nil))

  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  int.power(-2, -1.0)
  |> should.equal(Ok(-0.5))
}

pub fn square_root_test() {
  int.square_root(4)
  |> should.equal(Ok(2.0))

  int.square_root(16)
  |> should.equal(Ok(4.0))

  int.square_root(0)
  |> should.equal(Ok(0.0))

  int.square_root(-4)
  |> should.equal(Error(Nil))
}

pub fn negate_test() {
  int.negate(-1)
  |> should.equal(1)

  int.negate(2)
  |> should.equal(-2)

  int.negate(0)
  |> should.equal(0)
}

pub fn sum_test() {
  int.sum([])
  |> should.equal(0)

  int.sum([1, 2, 3])
  |> should.equal(6)
}

pub fn product_test() {
  int.product([])
  |> should.equal(0)

  int.product([4])
  |> should.equal(4)

  int.product([1, 2, 3])
  |> should.equal(6)
}

pub fn digits_test() {
  int.digits(123, 10)
  |> should.equal(Ok([1, 2, 3]))

  int.digits(-123, 10)
  |> should.equal(Ok([-1, -2, -3]))

  int.digits(123, 2)
  |> should.equal(Ok([1, 1, 1, 1, 0, 1, 1]))

  int.digits(123, 1)
  |> should.equal(Error(int.InvalidBase))
}

pub fn undigits_test() {
  int.undigits([], 10)
  |> should.equal(Ok(0))

  int.undigits([1, 2, 3], 10)
  |> should.equal(Ok(123))

  int.undigits([-1, -2, -3], 10)
  |> should.equal(Ok(-123))

  int.undigits([1, 1, 1, 1, 0, 1, 1], 2)
  |> should.equal(Ok(123))

  int.undigits([1, 2, 3], 1)
  |> should.equal(Error(int.InvalidBase))

  int.undigits([1, 1, 2], 2)
  |> should.equal(Error(int.InvalidBase))
}

pub fn random_test() {
  let test_boundaries = fn(_accumulator, _element) {
    int.random(0, 0)
    |> should.equal(0)

    int.random(-1, 0)
    |> list.contains([-1, 0], _)
    |> should.be_true

    int.random(-1, 1)
    |> list.contains([-1, 0], _)
    |> should.be_true

    int.random(-1, 2)
    |> list.contains([-1, 0, 1], _)
    |> should.be_true
  }
  list.range(0, 100)
  |> iterator.from_list
  |> iterator.fold(Nil, test_boundaries)

  let test_average = fn(iterations: Int, min: Int, max: Int, tolerance: Int) {
    let expected_average = int.sum([min, max]) / 2
    list.range(0, iterations)
    |> iterator.from_list
    |> iterator.fold(
      from: 0,
      with: fn(accumulator, _element) { accumulator + int.random(min, max) },
    )
    |> fn(sum) { sum / iterations }
    |> fn(average) {
      average - tolerance <= expected_average || average + tolerance >= expected_average
    }
    |> should.be_true
  }
  test_average(100, 0, 0, 5)
  test_average(1_000, 0, 100, 5)
  test_average(1_000, -100, 100, 5)
  test_average(1_000, -100, 0, 5)
  test_average(1_000, 0, -100, 5)
}

pub fn divide_test() {
  int.divide(1, 1)
  |> should.equal(Ok(1))

  int.divide(1, 0)
  |> should.equal(Error(Nil))

  int.divide(0, by: 1)
  |> should.equal(Ok(0))

  int.divide(1, by: 0)
  |> should.equal(Error(Nil))

  int.divide(5, by: 2)
  |> should.equal(Ok(2))

  int.divide(-99, by: 2)
  |> should.equal(Ok(-49))
}

pub fn remainder_test() {
  int.remainder(3, 2)
  |> should.equal(Ok(1))

  int.remainder(1, 0)
  |> should.equal(Error(Nil))

  int.remainder(10, -1)
  |> should.equal(Ok(0))

  int.remainder(13, by: 3)
  |> should.equal(Ok(1))

  int.remainder(-13, by: 3)
  |> should.equal(Ok(-1))

  int.remainder(13, by: -3)
  |> should.equal(Ok(1))

  int.remainder(-13, by: -3)
  |> should.equal(Ok(-1))
}

pub fn modulo_test() {
  int.modulo(3, 2)
  |> should.equal(Ok(1))

  int.modulo(1, 0)
  |> should.equal(Error(Nil))

  int.modulo(10, -1)
  |> should.equal(Ok(0))

  int.modulo(13, by: 3)
  |> should.equal(Ok(1))

  int.modulo(-13, by: 3)
  |> should.equal(Ok(2))

  int.modulo(13, by: -3)
  |> should.equal(Ok(-2))

  int.modulo(-13, by: -3)
  |> should.equal(Ok(-1))
}

pub fn floor_divide_test() {
  int.floor_divide(1, 1)
  |> should.equal(Ok(1))

  int.floor_divide(1, 0)
  |> should.equal(Error(Nil))

  int.floor_divide(0, by: 1)
  |> should.equal(Ok(0))

  int.floor_divide(1, by: 0)
  |> should.equal(Error(Nil))

  int.floor_divide(5, by: 2)
  |> should.equal(Ok(2))

  int.floor_divide(6, by: -4)
  |> should.equal(Ok(-2))

  int.floor_divide(-99, by: 2)
  |> should.equal(Ok(-50))
}

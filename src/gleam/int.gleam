//// Functions for working with integers.
////
//// ## Division by zero
////
//// In Erlang division by zero results in a crash, however Gleam does not have
//// partial functions and operators in core so instead division by zero returns
//// zero, a behaviour taken from Pony, Coq, and Lean.
////
//// This may seem unexpected at first, but it is no less mathematically valid
//// than crashing or returning a special value. Division by zero is undefined
//// in mathematics.

import gleam/float
import gleam/order.{type Order}

/// Returns the absolute value of the input.
///
/// ## Examples
///
/// ```gleam
/// assert absolute_value(-12) == 12
/// ```
///
/// ```gleam
/// assert absolute_value(10) == 10
/// ```
///
pub fn absolute_value(x: Int) -> Int {
  case x >= 0 {
    True -> x
    False -> x * -1
  }
}

/// Returns the result of the base being raised to the power of the
/// exponent, as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// assert power(2, -1.0) == Ok(0.5)
/// ```
///
/// ```gleam
/// assert power(2, 2.0) == Ok(4.0)
/// ```
///
/// ```gleam
/// assert power(8, 1.5) == Ok(22.627416997969522)
/// ```
///
/// ```gleam
/// assert 4 |> power(of: 2.0) == Ok(16.0)
/// ```
///
/// ```gleam
/// assert power(-1, 0.5) == Error(Nil)
/// ```
///
pub fn power(base: Int, of exponent: Float) -> Result(Float, Nil) {
  base
  |> to_float
  |> float.power(exponent)
}

/// Returns the square root of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// assert square_root(4) == Ok(2.0)
/// ```
///
/// ```gleam
/// assert square_root(-16) == Error(Nil)
/// ```
///
pub fn square_root(x: Int) -> Result(Float, Nil) {
  x
  |> to_float
  |> float.square_root()
}

/// Parses a given string as an int if possible.
///
/// ## Examples
///
/// ```gleam
/// assert parse("2") == Ok(2)
/// ```
///
/// ```gleam
/// assert parse("ABC") == Error(Nil)
/// ```
///
@external(erlang, "gleam_stdlib", "parse_int")
@external(javascript, "../gleam_stdlib.mjs", "parse_int")
pub fn parse(string: String) -> Result(Int, Nil)

/// Parses a given string as an int in a given base if possible.
/// Supports only bases 2 to 36, for values outside of which this function returns an `Error(Nil)`.
///
/// ## Examples
///
/// ```gleam
/// assert base_parse("10", 2) == Ok(2)
/// ```
///
/// ```gleam
/// assert base_parse("30", 16) == Ok(48)
/// ```
///
/// ```gleam
/// assert base_parse("1C", 36) == Ok(48)
/// ```
///
/// ```gleam
/// assert base_parse("48", 1) == Error(Nil)
/// ```
///
/// ```gleam
/// assert base_parse("48", 37) == Error(Nil)
/// ```
///
pub fn base_parse(string: String, base: Int) -> Result(Int, Nil) {
  case base >= 2 && base <= 36 {
    True -> do_base_parse(string, base)
    False -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "int_from_base_string")
@external(javascript, "../gleam_stdlib.mjs", "int_from_base_string")
fn do_base_parse(a: String, b: Int) -> Result(Int, Nil)

/// Prints a given int to a string.
///
/// ## Examples
///
/// ```gleam
/// assert to_string(2) == "2"
/// ```
///
@external(erlang, "erlang", "integer_to_binary")
@external(javascript, "../gleam_stdlib.mjs", "to_string")
pub fn to_string(x: Int) -> String

/// Prints a given int to a string using the base number provided.
/// Supports only bases 2 to 36, for values outside of which this function returns an `Error(Nil)`.
/// For common bases (2, 8, 16, 36), use the `to_baseN` functions.
///
/// ## Examples
///
/// ```gleam
/// assert to_base_string(2, 2) == Ok("10")
/// ```
///
/// ```gleam
/// assert to_base_string(48, 16) == Ok("30")
/// ```
///
/// ```gleam
/// assert to_base_string(48, 36) == Ok("1C")
/// ```
///
/// ```gleam
/// assert to_base_string(48, 1) == Error(Nil)
/// ```
///
/// ```gleam
/// assert to_base_string(48, 37) == Error(Nil)
/// ```
///
pub fn to_base_string(x: Int, base: Int) -> Result(String, Nil) {
  case base >= 2 && base <= 36 {
    True -> Ok(do_to_base_string(x, base))
    False -> Error(Nil)
  }
}

@external(erlang, "erlang", "integer_to_binary")
@external(javascript, "../gleam_stdlib.mjs", "int_to_base_string")
fn do_to_base_string(a: Int, b: Int) -> String

/// Prints a given int to a string using base-2.
///
/// ## Examples
///
/// ```gleam
/// assert to_base2(2) == "10"
/// ```
///
pub fn to_base2(x: Int) -> String {
  do_to_base_string(x, 2)
}

/// Prints a given int to a string using base-8.
///
/// ## Examples
///
/// ```gleam
/// assert to_base8(15) == "17"
/// ```
///
pub fn to_base8(x: Int) -> String {
  do_to_base_string(x, 8)
}

/// Prints a given int to a string using base-16.
///
/// ## Examples
///
/// ```gleam
/// assert to_base16(48) == "30"
/// ```
///
pub fn to_base16(x: Int) -> String {
  do_to_base_string(x, 16)
}

/// Prints a given int to a string using base-36.
///
/// ## Examples
///
/// ```gleam
/// assert to_base36(48) == "1C"
/// ```
///
pub fn to_base36(x: Int) -> String {
  do_to_base_string(x, 36)
}

/// Takes an int and returns its value as a float.
///
/// ## Examples
///
/// ```gleam
/// assert to_float(5) == 5.0
/// ```
///
/// ```gleam
/// assert to_float(0) == 0.0
/// ```
///
/// ```gleam
/// assert to_float(-3) == -3.0
/// ```
///
@external(erlang, "erlang", "float")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn to_float(x: Int) -> Float

/// Restricts an int between two bounds.
///
/// Note: If the `min` argument is larger than the `max` argument then they
/// will be swapped, so the minimum bound is always lower than the maximum
/// bound.
///
/// ## Examples
///
/// ```gleam
/// assert clamp(40, min: 50, max: 60) == 50
/// ```
///
/// ```gleam
/// assert clamp(40, min: 50, max: 30) == 40
/// ```
///
pub fn clamp(x: Int, min min_bound: Int, max max_bound: Int) -> Int {
  case min_bound >= max_bound {
    True -> x |> min(min_bound) |> max(max_bound)
    False -> x |> min(max_bound) |> max(min_bound)
  }
}

/// Compares two ints, returning an order.
///
/// ## Examples
///
/// ```gleam
/// assert compare(2, 3) == Lt
/// ```
///
/// ```gleam
/// assert compare(4, 3) == Gt
/// ```
///
/// ```gleam
/// assert compare(3, 3) == Eq
/// ```
///
pub fn compare(a: Int, with b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

/// Compares two ints, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// assert min(2, 3) == 2
/// ```
///
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Compares two ints, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// assert max(2, 3) == 3
/// ```
///
pub fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

/// Returns whether the value provided is even.
///
/// ## Examples
///
/// ```gleam
/// assert is_even(2)
/// ```
///
/// ```gleam
/// assert !is_even(3)
/// ```
///
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

/// Returns whether the value provided is odd.
///
/// ## Examples
///
/// ```gleam
/// assert is_odd(3)
/// ```
///
/// ```gleam
/// assert !is_odd(2)
/// ```
///
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}

/// Returns the negative of the value provided.
///
/// ## Examples
///
/// ```gleam
/// assert negate(1) == -1
/// ```
///
pub fn negate(x: Int) -> Int {
  -1 * x
}

/// Sums a list of ints.
///
/// ## Example
///
/// ```gleam
/// assert sum([1, 2, 3]) == 6
/// ```
///
pub fn sum(numbers: List(Int)) -> Int {
  sum_loop(numbers, 0)
}

fn sum_loop(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [first, ..rest] -> sum_loop(rest, first + initial)
    [] -> initial
  }
}

/// Multiplies a list of ints and returns the product.
///
/// ## Example
///
/// ```gleam
/// assert product([2, 3, 4]) == 24
/// ```
///
pub fn product(numbers: List(Int)) -> Int {
  product_loop(numbers, 1)
}

fn product_loop(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [first, ..rest] -> product_loop(rest, first * initial)
    [] -> initial
  }
}

/// Generates a random int between zero and the given maximum.
///
/// The lower number is inclusive, the upper number is exclusive.
///
/// ## Examples
///
/// ```gleam
/// random(10)
/// // -> 4
/// ```
///
/// ```gleam
/// random(1)
/// // -> 0
/// ```
///
/// ```gleam
/// random(-1)
/// // -> -1
/// ```
///
pub fn random(max: Int) -> Int {
  { float.random() *. to_float(max) }
  |> float.floor
  |> float.round
}

/// Performs a truncated integer division.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// assert divide(0, 1) == Ok(0)
/// ```
///
/// ```gleam
/// assert divide(1, 0) == Error(Nil)
/// ```
///
/// ```gleam
/// assert divide(5, 2) == Ok(2)
/// ```
///
/// ```gleam
/// assert divide(-99, 2) == Ok(-49)
/// ```
///
pub fn divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend / divisor)
  }
}

/// Computes the remainder of an integer division of inputs as a `Result`.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// Most of the time you will want to use the `%` operator instead of this
/// function.
///
/// ## Examples
///
/// ```gleam
/// assert remainder(3, 2) == Ok(1)
/// ```
///
/// ```gleam
/// assert remainder(1, 0) == Error(Nil)
/// ```
///
/// ```gleam
/// assert remainder(10, -1) == Ok(0)
/// ```
///
/// ```gleam
/// assert remainder(13, by: 3) == Ok(1)
/// ```
///
/// ```gleam
/// assert remainder(-13, by: 3) == Ok(-1)
/// ```
///
/// ```gleam
/// assert remainder(13, by: -3) == Ok(1)
/// ```
///
/// ```gleam
/// assert remainder(-13, by: -3) == Ok(-1)
/// ```
///
pub fn remainder(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend % divisor)
  }
}

/// Computes the modulo of an integer division of inputs as a `Result`.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// Note that this is different from `int.remainder` and `%` in that the
/// computed value will always have the same sign as the `divisor`.
///
/// ## Examples
///
/// ```gleam
/// assert modulo(3, 2) == Ok(1)
/// ```
///
/// ```gleam
/// assert modulo(1, 0) == Error(Nil)
/// ```
///
/// ```gleam
/// assert modulo(10, -1) == Ok(0)
/// ```
///
/// ```gleam
/// assert modulo(13, by: 3) == Ok(1)
/// ```
///
/// ```gleam
/// assert modulo(-13, by: 3) == Ok(2)
/// ```
///
/// ```gleam
/// assert modulo(13, by: -3) == Ok(-2)
/// ```
///
pub fn modulo(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    _ -> {
      let remainder = dividend % divisor
      case remainder * divisor < 0 {
        True -> Ok(remainder + divisor)
        False -> Ok(remainder)
      }
    }
  }
}

/// Performs a *floored* integer division, which means that the result will
/// always be rounded towards negative infinity.
///
/// If you want to perform truncated integer division (rounding towards zero),
/// use `int.divide()` or the `/` operator instead.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// assert floor_divide(1, 0) == Error(Nil)
/// ```
///
/// ```gleam
/// assert floor_divide(5, 2) == Ok(2)
/// ```
///
/// ```gleam
/// assert floor_divide(6, -4) == Ok(-2)
/// ```
///
/// ```gleam
/// assert floor_divide(-99, 2) == Ok(-50)
/// ```
///
pub fn floor_divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor ->
      case dividend * divisor < 0 && dividend % divisor != 0 {
        True -> Ok(dividend / divisor - 1)
        False -> Ok(dividend / divisor)
      }
  }
}

/// Adds two integers together.
///
/// It's the function equivalent of the `+` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// assert add(1, 2) == 3
/// ```
///
/// ```gleam
/// import gleam/list
/// assert list.fold([1, 2, 3], 0, add) == 6
/// ```
///
/// ```gleam
/// assert 3 |> add(2) == 5
/// ```
///
pub fn add(a: Int, b: Int) -> Int {
  a + b
}

/// Multiplies two integers together.
///
/// It's the function equivalent of the `*` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// assert multiply(2, 4) == 8
/// ```
///
/// ```gleam
/// import gleam/list
///
/// assert list.fold([2, 3, 4], 1, multiply) == 24
/// ```
///
/// ```gleam
/// assert 3 |> multiply(2) == 6
/// ```
///
pub fn multiply(a: Int, b: Int) -> Int {
  a * b
}

/// Subtracts one int from another.
///
/// It's the function equivalent of the `-` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// assert subtract(3, 1) == 2
/// ```
///
/// ```gleam
/// import gleam/list
///
/// assert list.fold([1, 2, 3], 10, subtract) == 4
/// ```
///
/// ```gleam
/// assert 3 |> subtract(2) == 1
/// ```
///
/// ```gleam
/// assert 3 |> subtract(2, _) == -1
/// ```
///
pub fn subtract(a: Int, b: Int) -> Int {
  a - b
}

/// Calculates the bitwise AND of its arguments.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "band")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_and")
pub fn bitwise_and(x: Int, y: Int) -> Int

/// Calculates the bitwise NOT of its argument.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "bnot")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_not")
pub fn bitwise_not(x: Int) -> Int

/// Calculates the bitwise OR of its arguments.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "bor")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_or")
pub fn bitwise_or(x: Int, y: Int) -> Int

/// Calculates the bitwise XOR of its arguments.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "bxor")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_exclusive_or")
pub fn bitwise_exclusive_or(x: Int, y: Int) -> Int

/// Calculates the result of an arithmetic left bitshift.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "bsl")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_shift_left")
pub fn bitwise_shift_left(x: Int, y: Int) -> Int

/// Calculates the result of an arithmetic right bitshift.
///
/// The exact behaviour of this function depends on the target platform.
/// On Erlang it is equivalent to bitwise operations on ints, on JavaScript it
/// is equivalent to bitwise operations on big-ints.
///
@external(erlang, "erlang", "bsr")
@external(javascript, "../gleam_stdlib.mjs", "bitwise_shift_right")
pub fn bitwise_shift_right(x: Int, y: Int) -> Int

/// Run a function for each int between ints `from` and `to`.
///
/// `from` is inclusive, and `to` is exclusive.
///
/// ## Examples
///
/// ```gleam
/// assert
///   range(from: 0, to: 3, with: "", run: fn(acc, i) {
///     acc <> to_string(i)
///   })
///   == "012"
/// ```
///
/// ```gleam
/// assert range(from: 1, to: -2, with: [], run: list.prepend) == [-1, 0, 1]
/// ```
///
pub fn range(
  from start: Int,
  to stop: Int,
  with acc: acc,
  run reducer: fn(acc, Int) -> acc,
) -> acc {
  let increment = case start < stop {
    True -> 1
    False -> -1
  }
  range_loop(start, stop, increment, acc, reducer)
}

fn range_loop(
  current: Int,
  stop: Int,
  increment: Int,
  acc: acc,
  reducer: fn(acc, Int) -> acc,
) -> acc {
  case current == stop {
    True -> acc
    False -> {
      let acc = reducer(acc, current)
      let current = current + increment
      range_loop(current, stop, increment, acc, reducer)
    }
  }
}

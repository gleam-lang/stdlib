import gleam/float
import gleam/order.{Order}

/// Returns the absolute value of the input.
///
/// ## Examples
///
/// ```gleam
/// > absolute_value(-12)
/// 12
///
/// > absolute_value(10)
/// 10
/// ```
///
pub fn absolute_value(num: Int) -> Int {
  case num >= 0 {
    True -> num
    False -> num * -1
  }
}

/// Parses a given string as an int if possible.
///
/// ## Examples
///
/// ```gleam
/// > parse("2")
/// Ok(2)
///
/// > parse("ABC")
/// Error(Nil)
/// ```
///
pub fn parse(string) {
  do_parse(string)
}

if erlang {
  external fn do_parse(String) -> Result(Int, Nil) =
    "gleam_stdlib" "parse_int"
}

if javascript {
  external fn do_parse(String) -> Result(Int, Nil) =
    "../gleam_stdlib.mjs" "parse_int"
}

/// Prints a given int to a string.
///
/// ## Examples
///
/// ```gleam
/// > to_string(2)
/// "2"
/// ```
///
pub fn to_string(int) {
  do_to_string(int)
}

if erlang {
  external fn do_to_string(Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_string(Int) -> String =
    "../gleam_stdlib.mjs" "to_string"
}

/// Error value when trying to operate with a base out of the allowed range.
pub type InvalidBase {
  InvalidBase
}

/// Prints a given int to a string using the base number provided.
/// Supports only bases 2 to 36, for values outside of which this function returns an `Error(InvalidBase)`.
/// For common bases (2, 8, 16, 36), use the `to_baseN` functions.
///
/// ## Examples
///
/// ```gleam
/// > to_base_string(2, 2)
/// Ok("10")
///
/// > to_base_string(48, 16)
/// Ok("30")
///
/// > to_base_string(48, 36)
/// Ok("1C")
///
/// > to_base_string(48, 1)
/// Error(InvalidBase)
///
/// > to_base_string(48, 37)
/// Error(InvalidBase)
/// ```
///
pub fn to_base_string(int, base) -> Result(String, InvalidBase) {
  case base >= 2 && base <= 36 {
    True -> Ok(do_to_base_string(int, base))
    False -> Error(InvalidBase)
  }
}

if erlang {
  external fn do_to_base_string(Int, Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_base_string(Int, Int) -> String =
    "../gleam_stdlib.mjs" "int_to_base_string"
}

/// Prints a given int to a string using base2.
///
/// ## Examples
///
/// ```gleam
/// > to_base2(2)
/// "10"
/// ```
///
pub fn to_base2(int) {
  do_to_base_string(int, 2)
}

/// Prints a given int to a string using base8.
///
/// ## Examples
///
/// ```gleam
/// > to_base8(15)
/// "17"
/// ```
///
pub fn to_base8(int) {
  do_to_base_string(int, 8)
}

/// Prints a given int to a string using base16.
///
/// ## Examples
///
/// ```gleam
/// > to_base16(48)
/// "30"
/// ```
///
pub fn to_base16(int) {
  do_to_base_string(int, 16)
}

/// Prints a given int to a string using base16.
///
/// ## Examples
///
/// ```gleam
/// > to_base36(48)
/// "1C"
/// ```
///
pub fn to_base36(int) {
  do_to_base_string(int, 36)
}

/// Takes an int and returns its value as a float.
///
/// ## Examples
///
/// ```
/// > to_float(5)
/// 5.
///
/// > to_float(0)
/// 0.
///
/// > to_float(-3)
/// -3.
/// ```
///
pub fn to_float(int) {
  do_to_float(int)
}

if erlang {
  external fn do_to_float(a: Int) -> Float =
    "erlang" "float"
}

if javascript {
  external fn do_to_float(a: Int) -> Float =
    "../gleam_stdlib.mjs" "identity"
}

/// Restricts an int between a lower and upper bound.
///
/// ## Examples
///
/// ```
/// > clamp(40, min: 50, max: 60)
/// 50
/// ```
///
pub fn clamp(n: Int, min min_bound: Int, max max_bound: Int) -> Int {
  n
  |> min(max_bound)
  |> max(min_bound)
}

/// Compares two ints, returning an order.
///
/// ## Examples
///
/// ```gleam
/// > compare(2, 3)
/// Lt
///
/// > compare(4, 3)
/// Gt
///
/// > compare(3, 3)
/// Eq
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
/// > min(2, 3)
/// 2
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
/// > max(2, 3)
/// 3
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
/// > is_even(2)
/// True
///
/// > is_even(3)
/// False
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
/// > is_odd(3)
/// True
///
/// > is_odd(2)
/// False
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
/// > negate(1)
/// -1
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
/// > sum([1, 2, 3])
/// 6
/// ```
///
pub fn sum(numbers: List(Int)) -> Int {
  numbers
  |> do_sum(0)
}

fn do_sum(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_sum(rest, x + initial)
  }
}

/// Multiplies a list of ints and returns the product.
///
/// ## Example
///
/// ```gleam
/// > product([2, 3, 4])
/// 24
/// ```
///
pub fn product(numbers: List(Int)) -> Int {
  case numbers {
    [] -> 0
    _ -> do_product(numbers, 1)
  }
}

fn do_product(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x * initial)
  }
}

/// Splits an integer into its digit representation in the specified base
///
/// ## Examples
///
/// ```gleam
/// > digits(234, 10)
/// Ok([2,3,4])
///
/// > digits(234, 1)
/// Error(InvalidBase)
/// ```
///
pub fn digits(number: Int, base: Int) -> Result(List(Int), InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> Ok(do_digits(number, base, []))
  }
}

fn do_digits(number: Int, base: Int, acc: List(Int)) -> List(Int) {
  case absolute_value(number) < base {
    True -> [number, ..acc]
    False -> do_digits(number / base, base, [number % base, ..acc])
  }
}

/// Joins a list of digits into a single value.
/// Returns an error if the base is less than 2 or if the list contains a digit greater than or equal to the specified base.
///
/// ## Examples
///
/// ```gleam
/// > undigits([2,3,4], 10)
/// Ok(234)
///
/// > undigits([2,3,4], 1)
/// Error(InvalidBase)
///
/// > undigits([2,3,4], 2)
/// Error(InvalidBase)
/// ```
///
pub fn undigits(numbers: List(Int), base: Int) -> Result(Int, InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> do_undigits(numbers, base, 0)
  }
}

fn do_undigits(
  numbers: List(Int),
  base: Int,
  acc: Int,
) -> Result(Int, InvalidBase) {
  case numbers {
    [] -> Ok(acc)
    [digit, ..] if digit >= base -> Error(InvalidBase)
    [digit, ..rest] -> do_undigits(rest, base, acc * base + digit)
  }
}

pub fn random(boundary_a: Int, boundary_b: Int) -> Int {
  // ```javascript
  // min = Math.ceil(min);
  // max = Math.floor(max);
  // return Math.floor(Math.random() * (max - min) + min); // The minimum is inclusive and the maximum is exclusive
  // ```
  // See: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random#getting_a_random_integer_between_two_values>
  let #(min, max) = case boundary_a, boundary_b {
    a, b if a <= b -> #(a, b)
    a, b if a > b -> #(b, a)
  }

  let min =
    to_float(min)
    |> float.ceiling()
  let max =
    to_float(max)
    |> float.floor()

  float.random(min, max)
  |> float.floor()
  |> float.round()
}

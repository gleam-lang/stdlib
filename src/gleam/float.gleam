import gleam/string_builder
import gleam/order.{Order}

pub type Float =
  Float

/// Attempts to parse a string as a float, returning `Error(Nil)` if it was not
/// possible.
///
/// ## Examples
///    > parse("2.3")
///    Some(2.3)
///
///    > parse("ABC")
///    None
///
pub external fn parse(String) -> Result(Float, Nil) =
  "gleam_stdlib" "parse_float"

/// Return the string representation of the provided float.
///
/// ## Examples
///    > to_string(2.3)
///    "2.3"
///
pub fn to_string(f: Float) -> String {
  f
  |> string_builder.from_float
  |> string_builder.to_string
}

/// Compares two floats, returning an order.
///
/// ## Examples
///    > compare(2.0, 2.3)
///    Lt
///
pub fn compare(a: Float, with b: Float) -> Order {
  case a == b {
    True -> order.Eq
    False -> case a <. b {
      True -> order.Lt
      False -> order.Gt
    }
  }
}

/// Compares two floats, returning the smaller of the two.
///
/// ## Examples
///
///    > min(2.0, 2.3)
///    2.0
///
pub fn min(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}

/// Compares two floats, returning the larger of the two.
///
/// ## Examples
///
///    > max(2.0, 2.3)
///    2.3
///
pub fn max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}

/// Rounds the value to the next highest whole number as a float.
///
/// ## Examples
///
///    > ceiling(2.3)
///    3.0
///
pub external fn ceiling(Float) -> Float =
  "math" "ceil"

/// Rounds the value to the next lowest whole number as a float.
///
/// ## Examples
///
///    > floor(2.3)
///    2.0
///
pub external fn floor(Float) -> Float =
  "math" "floor"

/// Rounds the value to the nearest whole number as an int.
///
/// ## Examples
///
///    > round(2.3)
///    2
///
///    > round(2.5)
///    3
///
pub external fn round(Float) -> Int =
  "erlang" "round"

/// Returns the value as an int, truncating all decimal digits.
///
/// ## Examples
///
///    > truncate(2.4343434847383438)
///    2
///
pub external fn truncate(Float) -> Int =
  "erlang" "trunc"

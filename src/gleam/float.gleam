import gleam/iodata
import gleam/order.{Order}
import gleam/result.{Option}

pub type Float =
  Float

/// Attempts to parse the String as a Float if possible
///
/// ## Examples
///    > parse("2.3")
///    Some(2.3)
///
///    > parse("ABC")
///    None
///
///
pub external fn parse(String) -> Option(Float)
  = "gleam_stdlib" "parse_float";

/// Returns the string representation of the provided
/// `Float` value
///
/// ## Examples
///    > to_string(2.3)
///    "2.3"
///
///
pub fn to_string(f: Float) -> String {
  f
  |> iodata.from_float
  |> iodata.to_string
}


/// Compares two `Floats`, returning an `Order`
///
/// ## Examples
///    > compare(2.0, 2.3)
///    Lt
///
///
pub fn compare(a: Float, b: Float) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a <. b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

/// Compares two `Floats`, returning the smaller of the two
///
/// ## Examples
///    > min(2.0, 2.3)
///    2.0
///
///
pub fn min(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}

/// Compares two `Floats`, returning the larger of the two
///
/// ## Examples
///    > max(2.0, 2.3)
///    2.3
///
///
pub fn max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}

/// Rounds the value to the next highest whole number as a Float
///
/// ## Examples
///    > ceiling(2.3)
///    3.0
///
///
pub external fn ceiling(Float) -> Float = "math" "ceil";

/// Rounds the value to the next lowest whole number as a Float
///
/// ## Examples
///    > floor(2.3)
///    2.0
///
///
pub external fn floor(Float) -> Float = "math" "floor";

/// Rounds the value to the nearest whole number as an Int
///
/// ## Examples
///    > round(2.3)
///    2
///
///    > round(2.5)
///    3
///
///
pub external fn round(Float) -> Int = "erlang" "round";

/// Returns the value as an Int, truncating all decimal digits.
///
/// ## Examples
///    > truncate(2.4343434847383438)
///    2
///
///
pub external fn truncate(Float) -> Int = "erlang" "trunc";

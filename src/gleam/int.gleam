import gleam/order.{Order}
import gleam/result.{Option}

pub type Int =
  Int

/// Attempts to parse the String as an Int if possible
///
/// ## Examples
///    > parse("2")
///    Some(2)
///
///    > parse("ABC")
///    None
///
///
pub external fn parse(String) -> Option(Int) = "gleam_stdlib" "parse_int";

/// Returns the string representation of the provided
/// `Int` value
///
/// ## Examples
///    > to_string(2)
///    "2"
///
///
pub external fn to_string(Int) -> String = "erlang" "integer_to_binary"

/// Returns the string representation of the provided
/// `Int` value in the base provided.
///
/// ## Examples
///    > to_base_string(2, 2)
///    "10"
///
///    > to_base_string(48, 16)
///    "30"
///
///    > to_base_string(48, 36)
///    "1C"
///
///
pub external fn to_base_string(Int, Int) -> String = "erlang" "integer_to_binary"

/// Compares two `Int`, returning an `Order`
///
/// ## Examples
///    > compare(2, 3)
///    Lt
///
///
pub fn compare(a: Int, b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

/// Compares two `Int`, returning the smaller of the two
///
/// ## Examples
///    > min(2, 3)
///    2
///
///
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Compares two `Int`, returning the larger of the two
///
/// ## Examples
///    > max(2, 3)
///    3
///
///
pub fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

/// Returns whether the value provided is even
///
/// ## Examples
///    > is_even(2)
///    True
///
///    > is_even(3)
///    False
///
///
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

/// Returns whether the value provided is odd
///
/// ## Examples
///    > is_odd(3)
///    True
///
///    > is_odd(2)
///    False
///
///
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}

import gleam/order.{Order}

pub type Int =
  Int

/// Parse a given string as an int if possible.
///
/// ## Examples
///
///    > parse("2")
///    Ok(2)
///
///    > parse("ABC")
///    Error(Nil)
///
pub external fn parse(String) -> Result(Int, Nil) =
  "gleam_stdlib" "parse_int"

/// Print a given int to a string.
///
/// ## Examples
///
///    > to_string(2)
///    "2"
///
pub external fn to_string(Int) -> String =
  "erlang" "integer_to_binary"

/// Print a given int to a string using the base number provided.
///
/// ## Examples
///
///    > to_base_string(2, 2)
///    "10"
///
///    > to_base_string(48, 16)
///    "30"
///
///    > to_base_string(48, 36)
///    "1C"
///
pub external fn to_base_string(Int, Int) -> String =
  "erlang" "integer_to_binary"

/// Compares two ints, returning an order.
///
/// ## Examples
///
///    > compare(2, 3)
///    Lt
///
///    > compare(4, 3)
///    Gt
///
///    > compare(3, 3)
///    Eq
///
pub fn compare(a: Int, with b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False -> case a < b {
      True -> order.Lt
      False -> order.Gt
    }
  }
}

/// Compares two int, returning the smaller of the two.
///
/// ## Examples
///
///    > min(2, 3)
///    2
///
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Compares two int, returning the larger of the two.
///
/// ## Examples
///
///    > max(2, 3)
///    3
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
///    > is_even(2)
///    True
///
///    > is_even(3)
///    False
///
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

/// Returns whether the value provided is odd.
///
/// ## Examples
///
///    > is_odd(3)
///    True
///
///    > is_odd(2)
///    False
///
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}

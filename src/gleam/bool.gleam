/// A set of utility functions for working with `Bool` values.

import gleam/order.{Order}

/// Returns the opposite Bool value
///
/// ## Examples
/// ```gleam
/// negate(True) == False
/// ```
///
pub fn negate(bool: Bool) -> Bool {
  case bool {
    True -> False
    False -> True
  }
}

/// Compares two bools and returns the first values Order to the second.
///
/// ## Examples
/// ```gleam
/// import gleam/order
/// compare(True, False) == order.Gt
/// ```
///
pub fn compare(a: Bool, b: Bool) -> Order {
  case a, b {
    True, True -> order.Eq
    True, False -> order.Gt
    False, False -> order.Eq
    False, True -> order.Lt
  }
}

/// Returns `True` if either Bool value is `True`.
///
/// ## Examples
/// ```gleam
/// max(True, False) == True
/// max(False, True) === True
/// max(False, False) === False
/// ```
///
pub fn max(a: Bool, b: Bool) -> Bool {
  case a {
    True -> True
    False -> b
  }
}

/// Returns `False` if either Bool value is `False`.
///
/// ## Examples
/// ```gleam
/// max(True, False) == False
/// max(False, True) === False
/// max(False, False) === False
/// ```
///
pub fn min(a: Bool, b: Bool) -> Bool {
  case a {
    False -> False
    True -> b
  }
}

/// Returns a numeric representation of the value:
///
/// ## Examples
/// ```gleam
/// to_int(True) == 1
/// to_int(False) == 0
/// ```
///
pub fn to_int(bool: Bool) -> Int {
  case bool {
    False -> 0
    True -> 1
  }
}

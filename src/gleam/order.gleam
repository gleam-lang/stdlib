/// Represents the result of a single comparison to determine the precise
/// ordering of two values.

pub type Order {
  Lt
  Eq
  Gt
}

/// Switches the evaluated ordering from one direction to the other
///
/// ## Examples
/// ```gleam
/// reverse(Lt) == Gt
/// ```
///
pub fn reverse(order: Order) -> Order {
  case order {
    Lt -> Gt
    Eq -> Eq
    Gt -> Lt
  }
}

/// Produces a numeric representation of the order
///
/// ## Examples
/// ```gleam
/// to_int(Lt) == -1
/// ```
///
pub fn to_int(order: Order) -> Int {
  case order {
    Lt -> -1
    Eq -> 0
    Gt -> 1
  }
}

/// Compares two Order values to one another, producing a new Order
///
/// ## Examples
/// ```gleam
/// compare(Eq, to: Lt) == Gt
/// ```
///
pub fn compare(a: Order, to b: Order) -> Order {
  case a, b {
    x, y if x == y -> Eq
    Lt, _ | Eq, Gt -> Lt
    _, _ -> Gt
  }
}

/// Returns the highest of two orders
///
/// ## Examples
/// ```gleam
/// max(Eq, Lt) == Eq
/// ```
///
pub fn max(a: Order, b: Order) -> Order {
  case a, b {
    Gt, _ -> Gt
    Eq, Lt -> Eq
    _, _ -> b
  }
}

/// Returns the lowest of two orders
///
/// ## Examples
/// ```gleam
/// min(Eq, Lt) == Lt
/// ```
///
pub fn min(a: Order, b: Order) -> Order {
  case a, b {
    Lt, _ -> Lt
    Eq, Gt -> Eq
    _, _ -> b
  }
}

/// A set of utility higher-order functions for working with functions.

/// Takes two functions and chains them together to form one function that takes
/// the input from the first and returns the output of the second.
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun1(a) |> fun2 }
}

/// Takes a function that takes two arguments and returns a new function that
/// takes the same two arguments, but in reverse order.
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}

/// A function that always returns its input value.
pub fn identity(x: a) -> a {
  x
}

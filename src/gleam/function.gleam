import gleam/dynamic.{Dynamic}

/// Takes two functions and chains them together to form one function that takes
/// the input from the first and returns the output of the second.
///
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}

/// Takes a function that takes two arguments and returns a new function that
/// takes the same two arguments, but in reverse order.
///
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}

/// A function that always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}

pub type Exception {
  Exited(Dynamic)
  Thrown(Dynamic)
  Errored(Dynamic)
}

/// Gleam doesn't offer any way to raise exceptions, but they may still occur
/// due to bugs when working with unsafe code, such as when calling Erlang
/// function.
///
/// This function will catch any error thrown and convert it into a result
/// rather than crashing the process.
///
pub external fn rescue(fn() -> a) -> Result(a, Exception) =
  "gleam_stdlib" "rescue"

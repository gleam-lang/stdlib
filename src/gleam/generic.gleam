// A function that returns exactly what it was given.
pub fn identity(a: a) -> a { a }

// A function that, given two values, ignores one and always returns the other.
pub fn always(_a: a, b: b) -> b { b }

// Takes a function that takes two arguments and returns a new function that
// takes the same two arguments, but in reverse order.
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}

// Takes two functions and chains them together to form one function that takes
// the input from the first and returns the output of the second.
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun1(a) |> fun2 }
}

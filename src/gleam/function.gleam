/// Takes two functions and chains them together to form one function that
/// takes the input from the first and returns the output of the second.
///
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}

/// Takes a function with arity two
/// and returns a curried equivalent:
/// `fn(a, b) -> c` becomes `fn(a) -> fn(b) -> c`.
///
pub fn curry2(fun: fn(a, b) -> value) {
  fn(a) { fn(b) { fun(a, b) } }
}

/// Takes a function with arity three
/// and returns a curried equivalent:
/// `fn(a, b, c) -> d` becomes `fn(a) -> fn(b) -> fn(c) -> d`.
///
pub fn curry3(fun: fn(a, b, c) -> value) {
  fn(a) { fn(b) { fn(c) { fun(a, b, c) } } }
}

/// Takes a function with arity four
/// and returns a curried equivalent.
///
pub fn curry4(fun: fn(a, b, c, d) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fun(a, b, c, d) } } } }
}

/// Takes a function with arity five
/// and returns a curried equivalent.
pub fn curry5(fun: fn(a, b, c, d, e) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fn(e) { fun(a, b, c, d, e) } } } } }
}

/// Takes a function with arity six
/// and returns a curried equivalent.
///
pub fn curry6(fun: fn(a, b, c, d, e, f) -> value) {
  fn(a) {
    fn(b) { fn(c) { fn(d) { fn(e) { fn(f) { fun(a, b, c, d, e, f) } } } } }
  }
}

/// Takes a function that takes two arguments and returns a new function that
/// takes the same two arguments, but in reverse order.
///
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}

/// Takes a single argument and always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}

/// Takes a single argument and returns a new function that
/// ignores its argument and always returns the input value.
///
pub fn constant(value: a) -> fn(b) -> a {
  fn(_) { value }
}

/// Takes an argument and a single function,
/// calls that function with that argument
/// and returns that argument instead of the function return value.
/// Useful for running synchronous side effects in a pipeline.
///
pub fn tap(arg: a, effect: fn(a) -> b) -> a {
  effect(arg)
  arg
}

/// Takes a function with arity one and an argument,
/// calls that function with the argument and returns the function return value.
///
/// Useful for concisely calling functions returned as a part of a pipeline.
///
/// ## Example
///
/// ```gleam
/// > let doubler = fn() {
/// >  fn(x: Int) { x * 2 }
/// > }
/// >
/// > doubler()
/// > |> apply1(2)
/// 4
/// ```
///
pub fn apply1(fun: fn(a) -> value, arg1: a) -> value {
  fun(arg1)
}

/// Takes a function with arity two and two arguments,
/// calls that function with the arguments
/// and returns the function return value.
///
/// See `apply1` for more details.
///
pub fn apply2(fun: fn(a, b) -> value, arg1: a, arg2: b) -> value {
  fun(arg1, arg2)
}

/// Takes a function with arity three and three arguments,
/// calls that function with the arguments
/// and returns the function return value.
///
/// See `apply1` for more details.
///
pub fn apply3(fun: fn(a, b, c) -> value, arg1: a, arg2: b, arg3: c) -> value {
  fun(arg1, arg2, arg3)
}

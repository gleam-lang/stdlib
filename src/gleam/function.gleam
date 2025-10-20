/// Takes a single argument and always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}

/// Takes an argument and a single function, calls that function with that
/// argument and returns that argument instead of the function return value.
///
/// Useful for running synchronous side effects in a pipeline.
///
@deprecated("This function has been deprecated. You can bind the value you want to tap to a variable instead.")
pub fn tap(arg: a, effect: fn(a) -> b) -> a {
  effect(arg)
  arg
}

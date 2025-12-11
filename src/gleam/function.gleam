/// Takes a single argument and always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}

pub type ContinueOrStop(a) {
  Continue(a)
  Stop(a)
}

pub fn loop(
  from state: state,
  run function: fn(state) -> ContinueOrStop(state),
) -> state {
  case function(state) {
    Continue(state) -> loop(state, function)
    Stop(state) -> state
  }
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

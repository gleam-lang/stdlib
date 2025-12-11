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

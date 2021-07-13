//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

if erlang {
  // TODO: Move this module into another package so it can be used as a
  // dep only in test.
  pub external fn equal(a, a) -> Nil =
    "gleam_stdlib" "should_equal"

  pub external fn not_equal(a, a) -> Nil =
    "gleam_stdlib" "should_not_equal"

  pub external fn be_ok(Result(a, b)) -> Nil =
    "gleam_stdlib" "should_be_ok"

  pub external fn be_error(Result(a, b)) -> Nil =
    "gleam_stdlib" "should_be_error"
}

if javascript {
  pub fn equal(a, b) {
    assert True = a == b
    Nil
  }

  pub fn not_equal(a, b) {
    assert True = a != b
    Nil
  }

  pub fn be_ok(a) {
    assert Ok(_) = a
    Nil
  }

  pub fn be_error(a) {
    assert Error(_) = a
    Nil
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

pub fn fail() -> Nil {
  be_true(False)
}

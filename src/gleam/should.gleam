//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

// TODO: Move this module into another package so it can be used as a
// dep only in test.
pub external type Expectation

pub external fn equal(a, a) -> Expectation =
  "gleam_stdlib" "should_equal"

pub external fn not_equal(a, a) -> Expectation =
  "gleam_stdlib" "should_not_equal"

pub fn be_true(actual: Bool) -> Expectation {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Expectation {
  actual
  |> equal(False)
}

pub external fn be_ok(Result(a, b)) -> Expectation =
  "gleam_stdlib" "should_be_ok"

pub external fn be_error(Result(a, b)) -> Expectation =
  "gleam_stdlib" "should_be_error"

pub fn fail() -> Expectation {
  be_true(False)
}

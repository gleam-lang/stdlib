@target(erlang)
pub external fn main() -> Nil =
  "gleam_stdlib_test_ffi" "main"

@target(javascript)
pub external fn main() -> Nil =
  "././gleeunit.mjs" "main"

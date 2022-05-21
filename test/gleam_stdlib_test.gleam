if erlang {
  import gleam/string

  pub external fn main() -> Nil =
    "gleam_stdlib_test_ffi" "main"

  pub fn stdout(value) {
    value
    |> string.inspect
    |> fn(s) { string.concat(["\n", s]) }
    |> to_stdout
  }

  pub external fn to_stdout(string) -> Nil =
    "gleam_stdlib_test_ffi" "to_stdout"
}

if javascript {
  import gleam/string
  import gleam/io

  pub external fn main() -> Nil =
    "./gleam_stdlib_test_ffi.mjs" "main"

  pub fn stdout(value) {
    value
    |> string.inspect
    |> fn(s) { string.concat(["\n", s]) }
    |> io.println
  }
}

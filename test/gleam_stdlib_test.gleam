if erlang {
  pub external fn main() -> Nil =
    "gleam_stdlib_test_ffi" "main"
}

if javascript {
  pub external fn main() -> Nil =
    "./gleam_stdlib_test_ffi.mjs" "main"
}

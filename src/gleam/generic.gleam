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

/// Compares two values for equality.
///
/// ## Examples
///
/// ```gleam
/// > equal(True, False)
/// False
/// ```
///
/// ```gleam
/// > equal(False, False)
/// True
/// ```
///
/// ```gleam
/// > equal(1, 1)
/// True
/// ```
///
/// ```gleam
/// > "Gleam" |> equal(to: "Erlang")
/// False
/// ```
///
pub fn equal(a: any, to b: any) -> Bool {
  a == b
}

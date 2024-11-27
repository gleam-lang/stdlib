import gleam/string

/// Writes a string to standard output (stdout).
///
/// If you want your output to be printed on its own line see `println`.
///
/// ## Example
///
/// ```gleam
/// io.print("Hi mum")
/// // -> Nil
/// // Hi mum
/// ```
///
@external(erlang, "gleam_stdlib", "print")
@external(javascript, "../gleam_stdlib.mjs", "print")
pub fn print(string: String) -> Nil

/// Writes a string to standard error (stderr).
///
/// If you want your output to be printed on its own line see `println_error`.
///
/// ## Example
///
/// ```
/// io.print_error("Hi pop")
/// // -> Nil
/// // Hi pop
/// ```
///
@external(erlang, "gleam_stdlib", "print_error")
@external(javascript, "../gleam_stdlib.mjs", "print_error")
pub fn print_error(string: String) -> Nil

/// Writes a string to standard output (stdout), appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// io.println("Hi mum")
/// // -> Nil
/// // Hi mum
/// ```
///
@external(erlang, "gleam_stdlib", "println")
@external(javascript, "../gleam_stdlib.mjs", "console_log")
pub fn println(string: String) -> Nil

/// Writes a string to standard error (stderr), appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// io.println_error("Hi pop")
/// // -> Nil
/// // Hi pop
/// ```
///
@external(erlang, "gleam_stdlib", "println_error")
@external(javascript, "../gleam_stdlib.mjs", "console_error")
pub fn println_error(string: String) -> Nil

/// Writes a value to standard error (stderr) yielding Gleam syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
/// ```gleam
/// debug("Hi mum")
/// // -> "Hi mum"
/// // <<"Hi mum">>
/// ```
///
/// ```gleam
/// debug(Ok(1))
/// // -> Ok(1)
/// // {ok, 1}
/// ```
///
/// ```gleam
/// import gleam/list
///
/// [1, 2]
/// |> list.map(fn(x) { x + 1 })
/// |> debug
/// |> list.map(fn(x) { x * 2 })
/// // -> [4, 6]
/// // [2, 3]
/// ```
///
pub fn debug(term: anything) -> anything {
  term
  |> string.inspect
  |> do_debug_println

  term
}

@external(erlang, "gleam_stdlib", "println_error")
@external(javascript, "../gleam_stdlib.mjs", "print_debug")
fn do_debug_println(string string: String) -> Nil

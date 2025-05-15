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

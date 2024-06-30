import gleam/string

/// Writes a string to standard output.
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
pub fn print(string: String) -> Nil {
  do_print(string)
}

@external(erlang, "gleam_stdlib", "print")
@external(javascript, "../gleam_stdlib.mjs", "print")
fn do_print(string string: String) -> Nil

/// Writes a string to standard error.
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
pub fn print_error(string: String) -> Nil {
  do_print_error(string)
}

@external(erlang, "gleam_stdlib", "print_error")
@external(javascript, "../gleam_stdlib.mjs", "print_error")
fn do_print_error(string string: String) -> Nil

/// Writes a string to standard output, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// io.println("Hi mum")
/// // -> Nil
/// // Hi mum
/// ```
///
pub fn println(string: String) -> Nil {
  do_println(string)
}

@external(erlang, "gleam_stdlib", "println")
@external(javascript, "../gleam_stdlib.mjs", "console_log")
fn do_println(string string: String) -> Nil

/// Writes a string to standard error, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// io.println_error("Hi pop")
/// // -> Nil
/// // Hi pop
/// ```
///
pub fn println_error(string: String) -> Nil {
  do_println_error(string)
}

@external(erlang, "gleam_stdlib", "println_error")
@external(javascript, "../gleam_stdlib.mjs", "console_error")
fn do_println_error(string string: String) -> Nil

/// Prints a value to standard error (stderr) yielding Gleam syntax.
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

/// Prints a string notating a value and the value itself to standard 
/// error (stderr) yielding Gleam syntax.
///
/// Only the value is returned after being printed so it can be used in 
/// pipelines.
///
/// ## Example
///
/// ```gleam
/// debug_note("Hi mum", with: "This is my greeting")
/// // -> "Hi mum"
/// // This is my greeting: "Hi mum"
/// ```
///
/// ```gleam
/// import gleam/list
///
/// [1, 2]
/// |> list.map(fn(x) { x + 1 })
/// |> debug_note("After adding 1")
/// |> list.map(fn(x) { x * 2 })
/// |> debug_note("After multiplying by 2")
/// // -> [4, 6]
/// // After adding 1: [2, 3]
/// // After multiplying by 2: [4, 6]
/// ```
///
pub fn debug_note(term: anything, with note: String) -> anything {
  { note <> ": " <> string.inspect(term) }
  |> do_debug_println

  term
}

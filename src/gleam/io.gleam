import gleam/string

/// Writes a string to standard output.
///
/// If you want your output to be printed on its own line see `println`.
///
/// ## Example
///
/// ```gleam
/// > io.print("Hi mum")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn print(string: String) -> Nil {
  do_print(string)
}

if erlang {
  external fn do_print(string: String) -> Nil =
    "gleam_stdlib" "print"
}

if javascript {
  external fn do_print(String) -> Nil =
    "../gleam_stdlib.mjs" "print"
}

/// Writes a string to standard error.
///
/// If you want your output to be printed on its own line see `eprintln`.
///
/// ## Example
///
/// ```
/// > io.eprint("Hi pop")
/// // -> Hi pop
/// Nil
/// ```
///
pub fn eprint(string: String) -> Nil {
  do_eprint(string)
}

if erlang {
  external fn do_eprint(string: String) -> Nil =
    "gleam_stdlib" "eprint"
}

if javascript {
  external fn do_eprint(String) -> Nil =
    "../gleam_stdlib.mjs" "eprint"
}

/// Writes a string to standard output, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// > io.println("Hi mum")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn println(string: String) -> Nil {
  do_println(string)
}

if erlang {
  external fn do_println(string: String) -> Nil =
    "gleam_stdlib" "println"
}

if javascript {
  external fn do_println(String) -> Nil =
    "../gleam_stdlib.mjs" "log"
}

/// Writes a string to standard error, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// > io.eprintln("Hi pop")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn eprintln(string: String) -> Nil {
  do_eprintln(string)
}

if erlang {
  external fn do_eprintln(string: String) -> Nil =
    "gleam_stdlib" "eprintln"
}

if javascript {
  external fn do_eprintln(String) -> Nil =
    "../gleam_stdlib.mjs" "error"
}

/// Prints a value to standard output (stdout) yielding Gleam syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
/// ```gleam
/// > debug("Hi mum")
/// // -> <<"Hi mum">>
/// "Hi mum"
/// ```
///
/// ```gleam
/// > debug(Ok(1))
/// // -> {ok, 1}
/// Ok(1)
/// ```
///
/// ```gleam
/// > import list
/// > [1, 2]
/// > |> list.map(fn(x) { x + 1 })
/// > |> debug
/// > |> list.map(fn(x) { x * 2 })
/// // -> [2, 3]
/// [4, 6]
/// ```
///
pub fn debug(term: anything) -> anything {
  term
  |> string.inspect
  |> println

  term
}

/// Prints a value to standard error (stderr) yielding Gleam syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
/// ```gleam
/// > io.edebug("Hi pop")
/// // -> <<"Hi pop">>
/// "Hi pop"
///
/// > io.edebug(Ok(1))
/// // -> {ok, 1}
/// Ok(1)
///
/// > import list
/// > [1, 2]
/// > |> list.map(fn(x) { x + 1 })
/// > |> io.edebug
/// > |> list.map(fn(x) { x * 2 })
/// // -> [2, 3]
/// [4, 6]
/// ```
///
pub fn edebug(term: anything) -> anything {
  term
  |> string.inspect
  |> eprintln

  term
}

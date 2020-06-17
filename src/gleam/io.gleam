external type DoNotLeak

external fn erl_print(String, List(a)) -> DoNotLeak =
  "io" "fwrite"

/// Writes a string to standard output.
///
/// ## Example
///
///    > io.print("Hi mum")
///    // -> Hi mum
///    Nil
///
pub fn print(string: String) -> Nil {
  erl_print(string, [])
  Nil
}

/// Writes a string to standard output, appending a newline to the end.
///
/// ## Example
///
///    > io.println("Hi mum")
///    // -> Hi mum
///    Nil
///
pub fn println(string: String) -> Nil {
  erl_print("~ts\n", [string])
  Nil
}

/// Print a value to standard output using Erlang syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
///    > io.debug("Hi mum")
///    // -> <<"Hi mum">>
///    "Hi mum"
///
///    > io.debug(Ok(1))
///    // -> {ok, 1}
///    Ok(1)
///
///    > import list
///    > [1, 2]
///    > |> list.map(fn(x) { x + 1 })
///    > |> io.debug
///    > |> list.map(fn(x) { x * 2 })
///    // -> [2, 3]
///    [4, 6]
///
pub fn debug(term: anything) -> anything {
  erl_print("~tp\n", [term])
  term
}

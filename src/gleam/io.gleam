if erlang {
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

  /// Prints a value to standard output using Erlang syntax.
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

  /// Error value returned by `get_line` function
  ///
  pub type GetLineError {
    Eof
    NoData
  }

  /// Reads a line from standard input with the given prompt.
  ///
  /// # Example
  ///
  ///    > io.get_line("Language: ")
  ///    // -> Language: <- gleam
  ///    Ok("gleam\n")
  ///
  pub external fn get_line(prompt: String) -> Result(String, GetLineError) =
    "gleam_stdlib" "get_line"
}

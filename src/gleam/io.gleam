external type DoNotLeak

external fn erl_print(String) -> DoNotLeak =
  "io" "fwrite"

/// Writes a string to standard output.
///
/// ## Example
///
///    > io.print("Hi mum")
///    Nil
///    //=> Hi mum
///   
pub fn print(string: String) -> Nil {
  erl_print(string)
  Nil
}

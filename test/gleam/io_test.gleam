if erlang {
  import gleam/io
  import gleam/should

  pub fn debug_test() {
    "io.debug-test"
    // prints to stdout, but EUnit will suppress that:
    |> io.debug()
    |> should.equal("io.debug-test")
  }
}

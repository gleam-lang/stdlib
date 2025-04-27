import gleam/function
import gleam/should
import gleam/string

pub fn identity_test() {
  1
  |> function.identity
  |> should.equal(1)

  ""
  |> function.identity
  |> should.equal("")

  []
  |> function.identity
  |> should.equal([])

  #(1, 2.0)
  |> function.identity
  |> should.equal(#(1, 2.0))
}

pub fn tap_test() {
  "Thanks Joe & Louis"
  |> function.tap(fn(s: String) {
    let _ = string.append(s, "... and Jose!")
    Nil
  })
  |> should.equal("Thanks Joe & Louis")
}

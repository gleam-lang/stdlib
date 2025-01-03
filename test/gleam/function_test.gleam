import gleam/function
import gleam/int
import gleam/should
import gleam/string

pub fn flip_test() {
  let fun = fn(s: String, i: Int) {
    s
    |> string.append("String: '", _)
    |> string.append("', Int: '")
    |> string.append(int.to_string(i))
    |> string.append("'")
  }

  let flipped_fun = function.flip(fun)

  fun("Bob", 1)
  |> should.equal("String: 'Bob', Int: '1'")

  flipped_fun(2, "Alice")
  |> should.equal("String: 'Alice', Int: '2'")
}

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
    string.append(s, "... and Jose!")
    Nil
  })
  |> should.equal("Thanks Joe & Louis")
}

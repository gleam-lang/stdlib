import gleam/should
import gleam/function
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn compose_test() {
  let add_two = fn(int: Int) { int + 2 }
  let add_three = fn(int: Int) { int + 3 }

  let add_five = function.compose(add_two, add_three)

  1
  |> add_five
  |> should.equal(6)

  // Takes a list of ints and returns the head as a string (if there is one, or
  // else "0" if there is not)
  let head_to_string = list.head
    |> function.compose(result.unwrap(_, 0))
    |> function.compose(int.to_string)

  [1]
  |> head_to_string
  |> should.equal("1")

  []
  |> head_to_string
  |> should.equal("0")
}

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

  tuple(1, 2.0)
  |> function.identity
  |> should.equal(tuple(1, 2.0))
}

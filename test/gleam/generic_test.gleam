import gleam/expect
import gleam/generic.{identity, always, flip, compose}
import gleam/int as int_mod
import gleam/list
import gleam/result
import gleam/string as string_mod

pub fn identity_test() {
  1
  |> identity
  |> expect.equal(_, 1)
}

pub fn always_test() {
  1
  |> always(_, 2)
  |> expect.equal(_, 2)
}

pub fn flip_test() {
  let fun = fn(string: String, int: Int) {
    string
    |> string_mod.append("String: '", _)
    |> string_mod.append(_, "', Int: '")
    |> string_mod.append(_, int_mod.to_string(int))
    |> string_mod.append(_, "'")
  }

  let flipped_fun = flip(fun)

  fun("Bob", 1)
  |> expect.equal(_, "String: 'Bob', Int: '1'")

  flipped_fun(2, "Alice")
  |> expect.equal(_, "String: 'Alice', Int: '2'")
}

pub fn compose_test() {
  let add_two = fn(int: Int) { int + 2 }
  let add_three = fn(int: Int) { int + 3 }

  let add_five = compose(add_two, add_three)

  1
  |> add_five
  |> expect.equal(_, 6)

  // Takes a list of ints and returns the head as a string (if there is one, or
  // else "0" if there is not)
  let head_to_string =
    compose(
      list.head,
      fn(int_result: Result(Int, Nil)) {
        result.unwrap(int_result, 0)
        |> int_mod.to_string
      }
    )

  [1]
  |> head_to_string
  |> expect.equal(_, "1")

  []
  |> head_to_string
  |> expect.equal(_, "0")
}

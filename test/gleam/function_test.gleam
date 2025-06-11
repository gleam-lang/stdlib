import gleam/function
import gleam/string

pub fn identity_test() {
  assert function.identity(1) == 1

  assert function.identity("") == ""

  assert function.identity([]) == []

  assert function.identity(#(1, 2.0)) == #(1, 2.0)
}

pub fn tap_test() {
  assert function.tap("Thanks Joe & Louis", fn(s: String) {
      let _ = string.append(s, "... and Jose!")
      Nil
    })
    == "Thanks Joe & Louis"
}

import gleam/function
import gleam/string

pub fn identity_test() {
  assert 1
    |> function.identity
    == 1

  assert ""
    |> function.identity
    == ""

  assert []
    |> function.identity
    == []

  assert #(1, 2.0)
    |> function.identity
    == #(1, 2.0)
}

pub fn tap_test() {
  assert "Thanks Joe & Louis"
    |> function.tap(fn(s: String) {
      let _ = string.append(s, "... and Jose!")
      Nil
    })
    == "Thanks Joe & Louis"
}

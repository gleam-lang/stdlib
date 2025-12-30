import gleam/function

pub fn identity_test() {
  assert function.identity(1) == 1
  assert function.identity("") == ""
  assert function.identity([]) == []
  assert function.identity(#(1, 2.0)) == #(1, 2.0)
}

import gleam/function

pub fn identity_int_test() {
  assert function.identity(1) == 1
}

pub fn identity_string_test() {
  assert function.identity("") == ""
}

pub fn identity_list_test() {
  assert function.identity([]) == []
}

pub fn identity_tuple_test() {
  assert function.identity(#(1, 2.0)) == #(1, 2.0)
}

import gleam/dynamic

pub fn classify_true_test() {
  assert dynamic.bool(True)
    |> dynamic.classify
    == "Bool"
}

pub fn classify_false_test() {
  assert dynamic.bool(False)
    |> dynamic.classify
    == "Bool"
}

pub fn null_test() {
  assert dynamic.nil()
    |> dynamic.classify
    == "Nil"
}

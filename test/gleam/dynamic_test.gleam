import gleam/dynamic

pub fn classify_true_test() {
  assert dynamic.classify(dynamic.bool(True)) == "Bool"
}

pub fn classify_false_test() {
  assert dynamic.classify(dynamic.bool(False)) == "Bool"
}

pub fn null_test() {
  assert dynamic.classify(dynamic.nil()) == "Nil"
}

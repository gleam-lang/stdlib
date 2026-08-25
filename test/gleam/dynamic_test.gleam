import gleam/dynamic

// On the native target booleans and Nil share the integer
// representation, so this cannot behave the same there.
@target(erlang)
pub fn classify_true_test() {
  assert dynamic.classify(dynamic.bool(True)) == "Bool"
}

@target(javascript)
pub fn classify_true_test() {
  assert dynamic.classify(dynamic.bool(True)) == "Bool"
}

// On the native target booleans and Nil share the integer
// representation, so this cannot behave the same there.
@target(erlang)
pub fn classify_false_test() {
  assert dynamic.classify(dynamic.bool(False)) == "Bool"
}

@target(javascript)
pub fn classify_false_test() {
  assert dynamic.classify(dynamic.bool(False)) == "Bool"
}

// On the native target booleans and Nil share the integer
// representation, so this cannot behave the same there.
@target(erlang)
pub fn null_test() {
  assert dynamic.classify(dynamic.nil()) == "Nil"
}

@target(javascript)
pub fn null_test() {
  assert dynamic.classify(dynamic.nil()) == "Nil"
}

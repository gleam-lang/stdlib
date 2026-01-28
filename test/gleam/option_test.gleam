import gleam/option.{None, Some}

pub fn all_some_test() {
  assert option.all([Some(1), Some(2), Some(3)]) == Some([1, 2, 3])
}

pub fn all_empty_test() {
  assert option.all([]) == Some([])
}

pub fn all_with_none_test() {
  assert option.all([Some(1), None, Some(3)]) == None
}

pub fn is_some_with_some_test() {
  assert option.is_some(Some(1))
}

pub fn is_some_with_none_test() {
  assert !option.is_some(None)
}

pub fn is_none_with_some_test() {
  assert !option.is_none(Some(1))
}

pub fn is_none_with_none_test() {
  assert option.is_none(None)
}

pub fn to_result_some_test() {
  assert option.to_result(Some(1), "possible_error") == Ok(1)
}

pub fn to_result_none_test() {
  assert option.to_result(None, "possible_error") == Error("possible_error")
}

pub fn from_result_ok_test() {
  assert option.from_result(Ok(1)) == Some(1)
}

pub fn from_result_error_test() {
  assert option.from_result(Error("some_error")) == None
}

pub fn unwrap_some_test() {
  assert option.unwrap(Some(1), 0) == 1
}

pub fn unwrap_none_test() {
  assert option.unwrap(None, 0) == 0
}

pub fn lazy_unwrap_some_test() {
  assert option.lazy_unwrap(Some(1), fn() { 0 }) == 1
}

pub fn lazy_unwrap_none_test() {
  assert option.lazy_unwrap(None, fn() { 0 }) == 0
}

pub fn map_some_int_test() {
  assert option.map(Some(1), fn(x) { x + 1 }) == Some(2)
}

pub fn map_some_type_change_test() {
  assert option.map(Some(1), fn(_) { "2" }) == Some("2")
}

pub fn map_none_test() {
  assert option.map(None, fn(x) { x + 1 }) == None
}

pub fn flatten_some_some_test() {
  assert option.flatten(Some(Some(1))) == Some(1)
}

pub fn flatten_some_none_test() {
  assert option.flatten(Some(None)) == None
}

pub fn flatten_none_test() {
  assert option.flatten(None) == None
}

pub fn then_some_to_some_test() {
  assert option.then(Some(1), fn(x) { Some(x + 1) }) == Some(2)
}

pub fn then_some_type_change_test() {
  assert option.then(Some(1), fn(_) { Some("2") }) == Some("2")
}

pub fn then_none_test() {
  assert option.then(None, fn(x) { Some(x + 1) }) == None
}

pub fn or_some_some_test() {
  assert option.or(Some(1), Some(2)) == Some(1)
}

pub fn or_some_none_test() {
  assert option.or(Some(1), None) == Some(1)
}

pub fn or_none_some_test() {
  assert option.or(None, Some(2)) == Some(2)
}

pub fn or_none_none_test() {
  assert option.or(None, None) == None
}

pub fn lazy_or_some_some_test() {
  assert option.lazy_or(Some(1), fn() { Some(2) }) == Some(1)
}

pub fn lazy_or_some_none_test() {
  assert option.lazy_or(Some(1), fn() { None }) == Some(1)
}

pub fn lazy_or_none_some_test() {
  assert option.lazy_or(None, fn() { Some(2) }) == Some(2)
}

pub fn lazy_or_none_none_test() {
  assert option.lazy_or(None, fn() { None }) == None
}

pub fn values_test() {
  assert option.values([Some(1), None, Some(3)]) == [1, 3]
}

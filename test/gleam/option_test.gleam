import gleam/option.{None, Some}

pub fn all_test() {
  assert option.all([Some(1), Some(2), Some(3)]) == Some([1, 2, 3])

  assert option.all([]) == Some([])

  assert option.all([Some(1), None, Some(3)]) == None
}

pub fn is_some_test() {
  assert option.is_some(Some(1))

  assert !option.is_some(None)
}

pub fn is_none_test() {
  assert !option.is_none(Some(1))

  assert option.is_none(None)
}

pub fn to_result_test() {
  assert option.to_result(Some(1), "possible_error") == Ok(1)

  assert option.to_result(None, "possible_error") == Error("possible_error")
}

pub fn from_result_test() {
  assert option.from_result(Ok(1)) == Some(1)

  assert option.from_result(Error("some_error")) == None
}

pub fn unwrap_option_test() {
  assert option.unwrap(Some(1), 0) == 1

  assert option.unwrap(None, 0) == 0
}

pub fn lazy_unwrap_option_test() {
  assert option.lazy_unwrap(Some(1), fn() { 0 }) == 1

  assert option.lazy_unwrap(None, fn() { 0 }) == 0
}

pub fn map_option_test() {
  assert Some(1)
    |> option.map(fn(x) { x + 1 })
    == Some(2)

  assert Some(1)
    |> option.map(fn(_) { "2" })
    == Some("2")

  assert None
    |> option.map(fn(x) { x + 1 })
    == None
}

pub fn flatten_option_test() {
  assert Some(Some(1))
    |> option.flatten()
    == Some(1)

  assert Some(None)
    |> option.flatten()
    == None

  assert None
    |> option.flatten()
    == None
}

pub fn then_option_test() {
  assert Some(1)
    |> option.then(fn(x) { Some(x + 1) })
    == Some(2)

  assert Some(1)
    |> option.then(fn(_) { Some("2") })
    == Some("2")

  assert None
    |> option.then(fn(x) { Some(x + 1) })
    == None
}

pub fn or_option_test() {
  assert Some(1)
    |> option.or(Some(2))
    == Some(1)

  assert Some(1)
    |> option.or(None)
    == Some(1)

  assert None
    |> option.or(Some(2))
    == Some(2)

  assert None
    |> option.or(None)
    == None
}

pub fn lazy_or_option_test() {
  assert Some(1)
    |> option.lazy_or(fn() { Some(2) })
    == Some(1)

  assert Some(1)
    |> option.lazy_or(fn() { None })
    == Some(1)

  assert None
    |> option.lazy_or(fn() { Some(2) })
    == Some(2)

  assert None
    |> option.lazy_or(fn() { None })
    == None
}

pub fn values_test() {
  assert option.values([Some(1), None, Some(3)]) == [1, 3]
}

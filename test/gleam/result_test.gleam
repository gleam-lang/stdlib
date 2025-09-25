import gleam/list
import gleam/result

pub fn is_ok_test() {
  assert result.is_ok(Ok(1))

  assert !result.is_ok(Error(1))
}

pub fn is_error_test() {
  assert !result.is_error(Ok(1))

  assert result.is_error(Error(1))
}

pub fn map_test() {
  assert result.map(Ok(1), fn(x) { x + 1 }) == Ok(2)

  assert result.map(Ok(1), fn(_) { "2" }) == Ok("2")

  assert result.map(Error(1), fn(x) { x + 1 }) == Error(1)
}

pub fn map_error_test() {
  assert result.map_error(Ok(1), fn(x) { x + 1 }) == Ok(1)

  assert result.map_error(Error(1), fn(x) { #("ok", x + 1) })
    == Error(#("ok", 2))
}

pub fn flatten_test() {
  assert result.flatten(Ok(Ok(1))) == Ok(1)

  assert result.flatten(Ok(Error(1))) == Error(1)

  assert result.flatten(Error(1)) == Error(1)

  assert result.flatten(Error(Error(1))) == Error(Error(1))
}

pub fn try_test() {
  assert result.try(Error(1), fn(x) { Ok(x + 1) }) == Error(1)

  assert result.try(Ok(1), fn(x) { Ok(x + 1) }) == Ok(2)

  assert result.try(Ok(1), fn(_) { Ok("type change") }) == Ok("type change")

  assert result.try(Ok(1), fn(_) { Error(1) }) == Error(1)
}

pub fn unwrap_test() {
  assert result.unwrap(Ok(1), 50) == 1

  assert result.unwrap(Error("nope"), 50) == 50
}

pub fn unwrap_error_test() {
  assert result.unwrap_error(Error(1), 50) == 1

  assert result.unwrap_error(Ok("nope"), 50) == 50
}

pub fn lazy_unwrap_test() {
  assert result.lazy_unwrap(Ok(1), fn() { 50 }) == 1

  assert result.lazy_unwrap(Error("nope"), fn() { 50 }) == 50
}

pub fn or_test() {
  assert result.or(Ok(1), Ok(2)) == Ok(1)

  assert result.or(Ok(1), Error("Error 2")) == Ok(1)

  assert result.or(Error("Error 1"), Ok(2)) == Ok(2)

  assert result.or(Error("Error 1"), Error("Error 2")) == Error("Error 2")
}

pub fn lazy_or_test() {
  assert result.lazy_or(Ok(1), fn() { Ok(2) }) == Ok(1)

  assert result.lazy_or(Ok(1), fn() { Error("Error 2") }) == Ok(1)

  assert result.lazy_or(Error("Error 1"), fn() { Ok(2) }) == Ok(2)

  assert result.lazy_or(Error("Error 1"), fn() { Error("Error 2") })
    == Error("Error 2")
}

pub fn all_test() {
  assert result.all([Ok(1), Ok(2), Ok(3)]) == Ok([1, 2, 3])

  assert result.all([Ok(1), Error("a"), Error("b"), Ok(3)]) == Error("a")
}

pub fn partition_test() {
  assert result.partition([]) == #([], [])

  assert result.partition([Ok(1), Ok(2), Ok(3)]) == #([3, 2, 1], [])

  assert result.partition([Error("a"), Error("b"), Error("c")])
    == #([], ["c", "b", "a"])

  assert result.partition([Ok(1), Error("a"), Ok(2), Error("b"), Error("c")])
    == #([2, 1], ["c", "b", "a"])

  // TCO test
  let _ =
    list.repeat(Ok(1), 1_000_000)
    |> result.partition

  list.repeat(Error("a"), 1_000_000)
  |> result.partition
}

pub fn replace_error_test() {
  assert result.replace_error(Error(Nil), "Invalid") == Error("Invalid")
}

pub fn replace_error_with_ok_test() {
  assert result.replace_error(Ok(Nil), "Invalid") == Ok(Nil)
}

pub fn replace_test() {
  assert result.replace(Ok(Nil), "OK") == Ok("OK")
}

pub fn replace_with_ok_test() {
  assert result.replace(Error(Nil), "Invalid") == Error(Nil)
}

pub fn values_test() {
  assert result.values([Ok(1), Error(""), Ok(3)]) == [1, 3]
}

pub fn try_recover_test() {
  assert result.try_recover(Ok(1), fn(_) { panic }) == Ok(1)

  assert result.try_recover(Error(1), fn(n) { Ok(n + 1) }) == Ok(2)

  assert result.try_recover(Error(1), fn(_) { Error("failed to recover") })
    == Error("failed to recover")
}

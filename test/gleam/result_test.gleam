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
  assert Ok(1)
    |> result.map(fn(x) { x + 1 })
    == Ok(2)

  assert Ok(1)
    |> result.map(fn(_) { "2" })
    == Ok("2")

  assert Error(1)
    |> result.map(fn(x) { x + 1 })
    == Error(1)
}

pub fn map_error_test() {
  assert Ok(1)
    |> result.map_error(fn(x) { x + 1 })
    == Ok(1)

  assert Error(1)
    |> result.map_error(fn(x) { #("ok", x + 1) })
    == Error(#("ok", 2))
}

pub fn flatten_test() {
  assert Ok(Ok(1))
    |> result.flatten
    == Ok(1)

  assert Ok(Error(1))
    |> result.flatten
    == Error(1)

  assert Error(1)
    |> result.flatten
    == Error(1)

  assert Error(Error(1))
    |> result.flatten
    == Error(Error(1))
}

pub fn try_test() {
  assert Error(1)
    |> result.try(fn(x) { Ok(x + 1) })
    == Error(1)

  assert Ok(1)
    |> result.try(fn(x) { Ok(x + 1) })
    == Ok(2)

  assert Ok(1)
    |> result.try(fn(_) { Ok("type change") })
    == Ok("type change")

  assert Ok(1)
    |> result.try(fn(_) { Error(1) })
    == Error(1)
}

pub fn then_test() {
  assert Error(1)
    |> result.then(fn(x) { Ok(x + 1) })
    == Error(1)

  assert Ok(1)
    |> result.then(fn(x) { Ok(x + 1) })
    == Ok(2)

  assert Ok(1)
    |> result.then(fn(_) { Ok("type change") })
    == Ok("type change")

  assert Ok(1)
    |> result.then(fn(_) { Error(1) })
    == Error(1)
}

pub fn unwrap_test() {
  assert Ok(1)
    |> result.unwrap(50)
    == 1

  assert Error("nope")
    |> result.unwrap(50)
    == 50
}

pub fn unwrap_error_test() {
  assert Error(1)
    |> result.unwrap_error(50)
    == 1

  assert Ok("nope")
    |> result.unwrap_error(50)
    == 50
}

pub fn unwrap_both_test() {
  assert Error(1)
    |> result.unwrap_both
    == 1

  assert Ok("yup")
    |> result.unwrap_both
    == "yup"
}

pub fn lazy_unwrap_test() {
  assert Ok(1)
    |> result.lazy_unwrap(fn() { 50 })
    == 1

  assert Error("nope")
    |> result.lazy_unwrap(fn() { 50 })
    == 50
}

pub fn or_test() {
  assert Ok(1)
    |> result.or(Ok(2))
    == Ok(1)

  assert Ok(1)
    |> result.or(Error("Error 2"))
    == Ok(1)

  assert Error("Error 1")
    |> result.or(Ok(2))
    == Ok(2)

  assert Error("Error 1")
    |> result.or(Error("Error 2"))
    == Error("Error 2")
}

pub fn lazy_or_test() {
  assert Ok(1)
    |> result.lazy_or(fn() { Ok(2) })
    == Ok(1)

  assert Ok(1)
    |> result.lazy_or(fn() { Error("Error 2") })
    == Ok(1)

  assert Error("Error 1")
    |> result.lazy_or(fn() { Ok(2) })
    == Ok(2)

  assert Error("Error 1")
    |> result.lazy_or(fn() { Error("Error 2") })
    == Error("Error 2")
}

pub fn all_test() {
  assert [Ok(1), Ok(2), Ok(3)]
    |> result.all
    == Ok([1, 2, 3])

  assert [Ok(1), Error("a"), Error("b"), Ok(3)]
    |> result.all
    == Error("a")
}

pub fn partition_test() {
  assert []
    |> result.partition
    == #([], [])

  assert [Ok(1), Ok(2), Ok(3)]
    |> result.partition
    == #([3, 2, 1], [])

  assert [Error("a"), Error("b"), Error("c")]
    |> result.partition
    == #([], ["c", "b", "a"])

  assert [Ok(1), Error("a"), Ok(2), Error("b"), Error("c")]
    |> result.partition
    == #([2, 1], ["c", "b", "a"])

  // TCO test
  let _ =
    list.repeat(Ok(1), 1_000_000)
    |> result.partition

  list.repeat(Error("a"), 1_000_000)
  |> result.partition
}

pub fn replace_error_test() {
  assert Error(Nil)
    |> result.replace_error("Invalid")
    == Error("Invalid")
}

pub fn replace_error_with_ok_test() {
  assert Ok(Nil)
    |> result.replace_error("Invalid")
    == Ok(Nil)
}

pub fn replace_test() {
  assert Ok(Nil)
    |> result.replace("OK")
    == Ok("OK")
}

pub fn replace_with_ok_test() {
  assert Error(Nil)
    |> result.replace("Invalid")
    == Error(Nil)
}

pub fn values_test() {
  assert result.values([Ok(1), Error(""), Ok(3)]) == [1, 3]
}

pub fn try_recover_test() {
  assert Ok(1)
    |> result.try_recover(fn(_) { panic })
    == Ok(1)

  assert Error(1)
    |> result.try_recover(fn(n) { Ok(n + 1) })
    == Ok(2)

  assert Error(1)
    |> result.try_recover(fn(_) { Error("failed to recover") })
    == Error("failed to recover")
}

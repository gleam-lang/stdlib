import gleam/list
import gleam/result
import gleam/should

pub fn is_ok_test() {
  result.is_ok(Ok(1))
  |> should.be_true

  result.is_ok(Error(1))
  |> should.be_false
}

pub fn is_error_test() {
  result.is_error(Ok(1))
  |> should.be_false

  result.is_error(Error(1))
  |> should.be_true
}

pub fn map_test() {
  Ok(1)
  |> result.map(fn(x) { x + 1 })
  |> should.equal(Ok(2))

  Ok(1)
  |> result.map(fn(_) { "2" })
  |> should.equal(Ok("2"))

  Error(1)
  |> result.map(fn(x) { x + 1 })
  |> should.equal(Error(1))
}

pub fn map_error_test() {
  Ok(1)
  |> result.map_error(fn(x) { x + 1 })
  |> should.equal(Ok(1))

  Error(1)
  |> result.map_error(fn(x) { #("ok", x + 1) })
  |> should.equal(Error(#("ok", 2)))
}

pub fn flatten_test() {
  Ok(Ok(1))
  |> result.flatten
  |> should.equal(Ok(1))

  Ok(Error(1))
  |> result.flatten
  |> should.equal(Error(1))

  Error(1)
  |> result.flatten
  |> should.equal(Error(1))

  Error(Error(1))
  |> result.flatten
  |> should.equal(Error(Error(1)))
}

pub fn try_test() {
  Error(1)
  |> result.try(fn(x) { Ok(x + 1) })
  |> should.equal(Error(1))

  Ok(1)
  |> result.try(fn(x) { Ok(x + 1) })
  |> should.equal(Ok(2))

  Ok(1)
  |> result.try(fn(_) { Ok("type change") })
  |> should.equal(Ok("type change"))

  Ok(1)
  |> result.try(fn(_) { Error(1) })
  |> should.equal(Error(1))
}

pub fn then_test() {
  Error(1)
  |> result.then(fn(x) { Ok(x + 1) })
  |> should.equal(Error(1))

  Ok(1)
  |> result.then(fn(x) { Ok(x + 1) })
  |> should.equal(Ok(2))

  Ok(1)
  |> result.then(fn(_) { Ok("type change") })
  |> should.equal(Ok("type change"))

  Ok(1)
  |> result.then(fn(_) { Error(1) })
  |> should.equal(Error(1))
}

pub fn unwrap_test() {
  Ok(1)
  |> result.unwrap(50)
  |> should.equal(1)

  Error("nope")
  |> result.unwrap(50)
  |> should.equal(50)
}

pub fn unwrap_error_test() {
  Error(1)
  |> result.unwrap_error(50)
  |> should.equal(1)

  Ok("nope")
  |> result.unwrap_error(50)
  |> should.equal(50)
}

pub fn unwrap_both_test() {
  Error(1)
  |> result.unwrap_both
  |> should.equal(1)

  Ok("yup")
  |> result.unwrap_both
  |> should.equal("yup")
}

pub fn lazy_unwrap_test() {
  Ok(1)
  |> result.lazy_unwrap(fn() { 50 })
  |> should.equal(1)

  Error("nope")
  |> result.lazy_unwrap(fn() { 50 })
  |> should.equal(50)
}

pub fn or_test() {
  Ok(1)
  |> result.or(Ok(2))
  |> should.equal(Ok(1))

  Ok(1)
  |> result.or(Error("Error 2"))
  |> should.equal(Ok(1))

  Error("Error 1")
  |> result.or(Ok(2))
  |> should.equal(Ok(2))

  Error("Error 1")
  |> result.or(Error("Error 2"))
  |> should.equal(Error("Error 2"))
}

pub fn lazy_or_test() {
  Ok(1)
  |> result.lazy_or(fn() { Ok(2) })
  |> should.equal(Ok(1))

  Ok(1)
  |> result.lazy_or(fn() { Error("Error 2") })
  |> should.equal(Ok(1))

  Error("Error 1")
  |> result.lazy_or(fn() { Ok(2) })
  |> should.equal(Ok(2))

  Error("Error 1")
  |> result.lazy_or(fn() { Error("Error 2") })
  |> should.equal(Error("Error 2"))
}

pub fn all_test() {
  [Ok(1), Ok(2), Ok(3)]
  |> result.all
  |> should.equal(Ok([1, 2, 3]))

  [Ok(1), Error("a"), Error("b"), Ok(3)]
  |> result.all
  |> should.equal(Error("a"))
}

pub fn partition_test() {
  []
  |> result.partition
  |> should.equal(#([], []))

  [Ok(1), Ok(2), Ok(3)]
  |> result.partition
  |> should.equal(#([3, 2, 1], []))

  [Error("a"), Error("b"), Error("c")]
  |> result.partition
  |> should.equal(#([], ["c", "b", "a"]))

  [Ok(1), Error("a"), Ok(2), Error("b"), Error("c")]
  |> result.partition
  |> should.equal(#([2, 1], ["c", "b", "a"]))

  // TCO test
  let _ =
    list.repeat(Ok(1), 1_000_000)
    |> result.partition

  list.repeat(Error("a"), 1_000_000)
  |> result.partition
}

pub fn replace_error_test() {
  Error(Nil)
  |> result.replace_error("Invalid")
  |> should.equal(Error("Invalid"))
}

pub fn replace_error_with_ok_test() {
  Ok(Nil)
  |> result.replace_error("Invalid")
  |> should.equal(Ok(Nil))
}

pub fn replace_test() {
  Ok(Nil)
  |> result.replace("OK")
  |> should.equal(Ok("OK"))
}

pub fn replace_with_ok_test() {
  Error(Nil)
  |> result.replace("Invalid")
  |> should.equal(Error(Nil))
}

pub fn values_test() {
  result.values([Ok(1), Error(""), Ok(3)])
  |> should.equal([1, 3])
}

pub fn try_recover_test() {
  Ok(1)
  |> result.try_recover(fn(_) { panic })
  |> should.equal(Ok(1))

  Error(1)
  |> result.try_recover(fn(n) { Ok(n + 1) })
  |> should.equal(Ok(2))

  Error(1)
  |> result.try_recover(fn(_) { Error("failed to recover") })
  |> should.equal(Error("failed to recover"))
}

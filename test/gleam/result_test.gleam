import gleam/should
import gleam/result

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

pub fn map_result_test() {
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
  |> result.map_error(fn(x) { tuple("ok", x + 1) })
  |> should.equal(Error(tuple("ok", 2)))
}

pub fn flatten_result_test() {
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

pub fn then_result_test() {
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

pub fn unwrap_result_test() {
  Ok(1)
  |> result.unwrap(50)
  |> should.equal(1)

  Error("nope")
  |> result.unwrap(50)
  |> should.equal(50)
}

pub fn nil_error_test() {
  Error("error_string")
  |> result.nil_error
  |> should.equal(Error(Nil))

  Error(123)
  |> result.nil_error
  |> should.equal(Error(Nil))

  Ok(1)
  |> result.nil_error
  |> should.equal(Ok(1))
}

pub fn or_result_test() {
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

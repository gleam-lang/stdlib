import gleam/expect
import gleam/option

pub fn is_some_test() {
  option.is_some(option.Some(1))
  |> expect.true

  option.is_some(option.None)
  |> expect.false
}

pub fn is_none_test() {
  option.is_none(option.Some(1))
  |> expect.false

  option.is_none(option.None)
  |> expect.true
}

pub fn map_test() {
  option.Some(1)
  |> option.map(_, fn(x) { x + 1 })
  |> expect.equal(_, option.Some(2))

  option.Some(1)
  |> option.map(_, fn(_) { "2" })
  |> expect.equal(_, option.Some("2"))

  option.None
  |> option.map(_, fn(x) { x + 1 })
  |> expect.equal(_, option.None)
}

pub fn flatten_test() {
  option.Some(option.Some(1))
  |> option.flatten
  |> expect.equal(_, option.Some(1))

  option.Some(option.None)
  |> option.flatten
  |> expect.equal(_, option.None)

  option.None
  |> option.flatten
  |> expect.equal(_, option.None)

  option.None
  |> option.flatten
  |> expect.equal(_, option.None)
}

pub fn then_test() {
  option.None
  |> option.then(_, fn(x) { option.Some(x + 1) })
  |> expect.equal(_, option.None)

  option.Some(1)
  |> option.then(_, fn(x) { option.Some(x + 1) })
  |> expect.equal(_, option.Some(2))

  option.Some(1)
  |> option.then(_, fn(_) { option.Some("type change") })
  |> expect.equal(_, option.Some("type change"))

  option.Some(1)
  |> option.then(_, fn(_) { option.None })
  |> expect.equal(_, option.None)
}

pub fn unwrap_test() {
  option.Some(1)
  |> option.unwrap(_, 50)
  |> expect.equal(_, 1)

  option.None
  |> option.unwrap(_, 50)
  |> expect.equal(_, 50)
}

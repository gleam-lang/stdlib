import gleam/should
import gleam/pair

pub fn first_test() {
  tuple(1, 2)
  |> pair.first
  |> should.equal(1)

  tuple("abc", [])
  |> pair.first
  |> should.equal("abc")
}

pub fn second_test() {
  tuple(1, 2)
  |> pair.second
  |> should.equal(2)

  tuple("abc", [])
  |> pair.second
  |> should.equal([])
}

pub fn swap_test() {
  tuple(1, "2")
  |> pair.swap
  |> should.equal(tuple("2", 1))
}

pub fn map_first_test() {
  let inc = fn(a) { a + 1 }
  pair.map_first(tuple(1, 2), inc)
  |> should.equal(tuple(2, 2))

  pair.map_first(tuple(8, 2), inc)
  |> should.equal(tuple(9, 2))

  pair.map_first(tuple(0, -2), inc)
  |> should.equal(tuple(1, -2))

  pair.map_first(tuple(-10, 20), inc)
  |> should.equal(tuple(-9, 20))
}

pub fn map_second_test() {
  let dec = fn(a) { a - 1 }
  pair.map_second(tuple(1, 2), dec)
  |> should.equal(tuple(1, 1))

  pair.map_second(tuple(8, 2), dec)
  |> should.equal(tuple(8, 1))

  pair.map_second(tuple(0, -2), dec)
  |> should.equal(tuple(0, -3))

  pair.map_second(tuple(-10, 20), dec)
  |> should.equal(tuple(-10, 19))
}

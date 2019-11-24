import gleam/expect
import gleam/pair

pub fn first_test() {
  struct(1, 2)
  |> pair.first
  |> expect.equal(_, 1)

  struct("abc", [])
  |> pair.first
  |> expect.equal(_, "abc")
}

pub fn second_test() {
  struct(1, 2)
  |> pair.second
  |> expect.equal(_, 2)

  struct("abc", [])
  |> pair.second
  |> expect.equal(_,[])
}

pub fn swap_test() {
  struct(1, "2")
  |> pair.swap
  |> expect.equal(_, struct("2", 1))
}

pub fn map_first_test() {
    let inc = fn(a) {
        a + 1
    }
    pair.map_first(struct(1, 2), inc)
    |> expect.equal(_, struct(2, 2))

    pair.map_first(struct(8,2), inc)
    |> expect.equal(_, struct(9, 2))

    pair.map_first(struct(0,-2), inc)
    |> expect.equal(_, struct(1, -2))

    pair.map_first(struct(-10, 20), inc)
    |> expect.equal(_, struct(-9, 20))
}

pub fn map_second_test() {
    let dec = fn(a) {
        a - 1
    }
    pair.map_second(struct(1, 2), dec)
    |> expect.equal(_, struct(1, 1))

    pair.map_second(struct(8,2), dec)
    |> expect.equal(_, struct(8, 1))

    pair.map_second(struct(0,-2), dec)
    |> expect.equal(_, struct(0, -3))

    pair.map_second(struct(-10, 20), dec)
    |> expect.equal(_, struct(-10, 19))
}

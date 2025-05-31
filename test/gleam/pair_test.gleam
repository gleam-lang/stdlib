import gleam/pair

pub fn first_test() {
  assert #(1, 2)
    |> pair.first
    == 1

  assert #("abc", [])
    |> pair.first
    == "abc"
}

pub fn second_test() {
  assert #(1, 2)
    |> pair.second
    == 2

  assert #("abc", [])
    |> pair.second
    == []
}

pub fn swap_test() {
  assert #(1, "2")
    |> pair.swap
    == #("2", 1)
}

pub fn map_first_test() {
  let inc = fn(a) { a + 1 }
  assert pair.map_first(#(1, 2), inc) == #(2, 2)

  assert pair.map_first(#(8, 2), inc) == #(9, 2)

  assert pair.map_first(#(0, -2), inc) == #(1, -2)

  assert pair.map_first(#(-10, 20), inc) == #(-9, 20)
}

pub fn map_second_test() {
  let dec = fn(a) { a - 1 }
  assert pair.map_second(#(1, 2), dec) == #(1, 1)

  assert pair.map_second(#(8, 2), dec) == #(8, 1)

  assert pair.map_second(#(0, -2), dec) == #(0, -3)

  assert pair.map_second(#(-10, 20), dec) == #(-10, 19)
}

pub fn new_test() {
  assert pair.new(1, 2) == #(1, 2)
}

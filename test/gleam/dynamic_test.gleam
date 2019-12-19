import gleam/dynamic
import gleam/atom
import gleam/list
import gleam/expect
import gleam/result
import gleam/map

pub fn string_test() {
  ""
  |> dynamic.from
  |> dynamic.string
  |> expect.equal(_, Ok(""))

  "Hello"
  |> dynamic.from
  |> dynamic.string
  |> expect.equal(_, Ok("Hello"))

  1
  |> dynamic.from
  |> dynamic.string
  |> expect.equal(_, Error("Expected a String, got `1`"))

  []
  |> dynamic.from
  |> dynamic.string
  |> expect.equal(_, Error("Expected a String, got `[]`"))
}

pub fn int_test() {
  1
  |> dynamic.from
  |> dynamic.int
  |> expect.equal(_, Ok(1))

  2
  |> dynamic.from
  |> dynamic.int
  |> expect.equal(_, Ok(2))

  1.0
  |> dynamic.from
  |> dynamic.int
  |> expect.equal(_, Error("Expected an Int, got `1.0`"))

  []
  |> dynamic.from
  |> dynamic.int
  |> expect.equal(_, Error("Expected an Int, got `[]`"))
}

pub fn float_test() {
  1.0
  |> dynamic.from
  |> dynamic.float
  |> expect.equal(_, Ok(1.0))

  2.2
  |> dynamic.from
  |> dynamic.float
  |> expect.equal(_, Ok(2.2))

  1
  |> dynamic.from
  |> dynamic.float
  |> expect.equal(_, Error("Expected a Float, got `1`"))

  []
  |> dynamic.from
  |> dynamic.float
  |> expect.equal(_, Error("Expected a Float, got `[]`"))
}

pub fn thunk_test() {
  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> expect.is_ok

  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> result.map(_, fn(f) { f() })
  |> expect.equal(_, Ok(dynamic.from(1)))

  fn(x) { x }
  |> dynamic.from
  |> dynamic.thunk
  |> expect.is_error

  1
  |> dynamic.from
  |> dynamic.thunk
  |> expect.is_error

  []
  |> dynamic.from
  |> dynamic.thunk
  |> expect.is_error
}

pub fn bool_test() {
  True
  |> dynamic.from
  |> dynamic.bool
  |> expect.equal(_, Ok(True))

  False
  |> dynamic.from
  |> dynamic.bool
  |> expect.equal(_, Ok(False))

  1
  |> dynamic.from
  |> dynamic.bool
  |> expect.equal(_, Error("Expected a Bool, got `1`"))

  []
  |> dynamic.from
  |> dynamic.bool
  |> expect.equal(_, Error("Expected a Bool, got `[]`"))
}

pub fn atom_test() {
  ""
    |> atom.create_from_string
    |> dynamic.from
    |> dynamic.atom
    |> expect.equal(_, Ok(atom.create_from_string("")))

  "ok"
    |> atom.create_from_string
    |> dynamic.from
    |> dynamic.atom
    |> expect.equal(_, Ok(atom.create_from_string("ok")))

  1
    |> dynamic.from
    |> dynamic.atom
    |> expect.is_error

  []
    |> dynamic.from
    |> dynamic.atom
    |> expect.is_error
}

pub fn list_test() {
  []
  |> dynamic.from
  |> dynamic.list(_, dynamic.string)
  |> expect.equal(_, Ok([]))

  []
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> expect.equal(_, Ok([]))

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> expect.equal(_, Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> dynamic.from
  |> dynamic.list(_, dynamic.list(_, dynamic.int))
  |> expect.equal(_, Ok([[1], [2], [3]]))

  1
  |> dynamic.from
  |> dynamic.list(_, dynamic.string)
  |> expect.is_error

  1.0
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> expect.is_error

  [""]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> expect.is_error

  [dynamic.from(1), dynamic.from("not an int")]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> expect.is_error
}

// TODO: struct2

pub fn field_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let Ok(error_atom) = atom.from_string("error")

  map.new()
  |> map.insert(_, ok_atom, 1)
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> expect.equal(_, Ok(dynamic.from(1)))

  map.new()
  |> map.insert(_, ok_atom, 3)
  |> map.insert(_, error_atom, 1)
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> expect.equal(_, Ok(dynamic.from(3)))

  map.new()
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> expect.is_error

  1
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> expect.is_error

  []
  |> dynamic.from
  |> dynamic.field(_, [])
  |> expect.is_error
}

pub fn element_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let Ok(error_atom) = atom.from_string("ok")
  let ok_one_struct = struct(ok_atom, 1)

  ok_one_struct
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> expect.equal(_, Ok(dynamic.from(ok_atom)))

  ok_one_struct
  |> dynamic.from
  |> dynamic.element(_, 1)
  |> expect.equal(_, Ok(dynamic.from(1)))

  ok_one_struct
  |> dynamic.from
  |> dynamic.element(_, 2)
  |> expect.is_error

  ok_one_struct
  |> dynamic.from
  |> dynamic.element(_, -1)
  |> expect.is_error

  1
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> expect.is_error

  map.new()
  |> map.insert(_, 1, ok_atom)
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> expect.is_error
}

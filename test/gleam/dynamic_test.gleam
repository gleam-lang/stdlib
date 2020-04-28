import gleam/dynamic
import gleam/atom
import gleam/list
import gleam/should
import gleam/result
import gleam/map

pub fn string_test() {
  ""
  |> dynamic.from
  |> dynamic.string
  |> should.equal(_, Ok(""))

  "Hello"
  |> dynamic.from
  |> dynamic.string
  |> should.equal(_, Ok("Hello"))

  1
  |> dynamic.from
  |> dynamic.string
  |> should.equal(_, Error("Expected a string, got an int"))

  []
  |> dynamic.from
  |> dynamic.string
  |> should.equal(_, Error("Expected a string, got a list"))
}

pub fn int_test() {
  1
  |> dynamic.from
  |> dynamic.int
  |> should.equal(_, Ok(1))

  2
  |> dynamic.from
  |> dynamic.int
  |> should.equal(_, Ok(2))

  1.0
  |> dynamic.from
  |> dynamic.int
  |> should.equal(_, Error("Expected an int, got a float"))

  []
  |> dynamic.from
  |> dynamic.int
  |> should.equal(_, Error("Expected an int, got a list"))
}

pub fn float_test() {
  1.0
  |> dynamic.from
  |> dynamic.float
  |> should.equal(_, Ok(1.0))

  2.2
  |> dynamic.from
  |> dynamic.float
  |> should.equal(_, Ok(2.2))

  1
  |> dynamic.from
  |> dynamic.float
  |> should.equal(_, Error("Expected a float, got an int"))

  []
  |> dynamic.from
  |> dynamic.float
  |> should.equal(_, Error("Expected a float, got a list"))
}

pub fn thunk_test() {
  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> should.be_ok

  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> result.map(_, fn(f) { f() })
  |> should.equal(_, Ok(dynamic.from(1)))

  fn(x) { x }
  |> dynamic.from
  |> dynamic.thunk
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.thunk
  |> should.be_error

  []
  |> dynamic.from
  |> dynamic.thunk
  |> should.be_error
}

pub fn bool_test() {
  True
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(_, Ok(True))

  False
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(_, Ok(False))

  1
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(_, Error("Expected a bool, got an int"))

  []
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(_, Error("Expected a bool, got a list"))
}

pub fn atom_test() {
  ""
    |> atom.create_from_string
    |> dynamic.from
    |> dynamic.atom
    |> should.equal(_, Ok(atom.create_from_string("")))

  "ok"
    |> atom.create_from_string
    |> dynamic.from
    |> dynamic.atom
    |> should.equal(_, Ok(atom.create_from_string("ok")))

  1
    |> dynamic.from
    |> dynamic.atom
    |> should.be_error

  []
    |> dynamic.from
    |> dynamic.atom
    |> should.be_error
}

pub fn list_test() {
  []
  |> dynamic.from
  |> dynamic.list(_, dynamic.string)
  |> should.equal(_, Ok([]))

  []
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> should.equal(_, Ok([]))

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> should.equal(_, Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> dynamic.from
  |> dynamic.list(_, dynamic.list(_, dynamic.int))
  |> should.equal(_, Ok([[1], [2], [3]]))

  1
  |> dynamic.from
  |> dynamic.list(_, dynamic.string)
  |> should.be_error

  1.0
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> should.be_error

  [""]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> should.be_error

  [dynamic.from(1), dynamic.from("not an int")]
  |> dynamic.from
  |> dynamic.list(_, dynamic.int)
  |> should.be_error
}

pub fn field_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let Ok(error_atom) = atom.from_string("error")

  map.new()
  |> map.insert(_, ok_atom, 1)
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> should.equal(_, Ok(dynamic.from(1)))

  map.new()
  |> map.insert(_, ok_atom, 3)
  |> map.insert(_, error_atom, 1)
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> should.equal(_, Ok(dynamic.from(3)))

  map.new()
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.field(_, ok_atom)
  |> should.be_error

  []
  |> dynamic.from
  |> dynamic.field(_, [])
  |> should.be_error
}

pub fn element_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let ok_one_tuple = tuple(ok_atom, 1)

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> should.equal(_, Ok(dynamic.from(ok_atom)))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(_, 1)
  |> should.equal(_, Ok(dynamic.from(1)))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(_, 2)
  |> should.be_error

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(_, -1)
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> should.be_error

  map.new()
  |> map.insert(_, 1, ok_atom)
  |> dynamic.from
  |> dynamic.element(_, 0)
  |> should.be_error
}

import gleam/bit_string
import gleam/dynamic
import gleam/atom
import gleam/list
import gleam/should
import gleam/result
import gleam/map
import gleam/option.{None, Some}

pub fn bit_string_test() {
  ""
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Ok(<<"":utf8>>))

  "Hello"
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Ok(<<"Hello":utf8>>))

  <<65535:16>>
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Ok(<<65535:16>>))

  1
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Error("Expected a bit_string, got an int"))

  []
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Error("Expected a bit_string, got a list"))
}

pub fn string_test() {
  ""
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Ok(""))

  "Hello"
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Ok("Hello"))

  <<65535:16>>
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Error("Expected a string, got a bit_string"))

  1
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Error("Expected a bit_string, got an int"))

  []
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Error("Expected a bit_string, got a list"))
}

pub fn int_test() {
  1
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Ok(1))

  2
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Ok(2))

  1.0
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Error("Expected an int, got a float"))

  []
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Error("Expected an int, got a list"))
}

pub fn float_test() {
  1.0
  |> dynamic.from
  |> dynamic.float
  |> should.equal(Ok(1.0))

  2.2
  |> dynamic.from
  |> dynamic.float
  |> should.equal(Ok(2.2))

  1
  |> dynamic.from
  |> dynamic.float
  |> should.equal(Error("Expected a float, got an int"))

  []
  |> dynamic.from
  |> dynamic.float
  |> should.equal(Error("Expected a float, got a list"))
}

pub fn thunk_test() {
  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> should.be_ok

  fn() { 1 }
  |> dynamic.from
  |> dynamic.thunk
  |> result.map(fn(f) { f() })
  |> should.equal(Ok(dynamic.from(1)))

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
  |> should.equal(Ok(True))

  False
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(Ok(False))

  1
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(Error("Expected a bool, got an int"))

  []
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(Error("Expected a bool, got a list"))
}

pub fn atom_test() {
  ""
  |> atom.create_from_string
  |> dynamic.from
  |> dynamic.atom
  |> should.equal(Ok(atom.create_from_string("")))

  "ok"
  |> atom.create_from_string
  |> dynamic.from
  |> dynamic.atom
  |> should.equal(Ok(atom.create_from_string("ok")))

  1
  |> dynamic.from
  |> dynamic.atom
  |> should.be_error

  []
  |> dynamic.from
  |> dynamic.atom
  |> should.be_error
}

pub fn typed_list_test() {
  []
  |> dynamic.from
  |> dynamic.typed_list(dynamic.string)
  |> should.equal(Ok([]))

  []
  |> dynamic.from
  |> dynamic.typed_list(dynamic.int)
  |> should.equal(Ok([]))

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.typed_list(dynamic.int)
  |> should.equal(Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> dynamic.from
  |> dynamic.typed_list(dynamic.typed_list(_, dynamic.int))
  |> should.equal(Ok([[1], [2], [3]]))

  1
  |> dynamic.from
  |> dynamic.typed_list(dynamic.string)
  |> should.be_error

  1.0
  |> dynamic.from
  |> dynamic.typed_list(dynamic.int)
  |> should.be_error

  [""]
  |> dynamic.from
  |> dynamic.typed_list(dynamic.int)
  |> should.be_error

  [dynamic.from(1), dynamic.from("not an int")]
  |> dynamic.from
  |> dynamic.typed_list(dynamic.int)
  |> should.be_error
}

pub fn option_test() {
  let Ok(null) = atom.from_string("null")

  1
  |> dynamic.from
  |> dynamic.option(dynamic.int)
  |> should.equal(Ok(Some(1)))
  null
  |> dynamic.from
  |> dynamic.option(dynamic.int)
  |> should.equal(Ok(None))
  1
  |> dynamic.from
  |> dynamic.option(dynamic.string)
  |> should.be_error
}

pub fn field_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let Ok(error_atom) = atom.from_string("error")

  map.new()
  |> map.insert(ok_atom, 1)
  |> dynamic.from
  |> dynamic.field(ok_atom)
  |> should.equal(Ok(dynamic.from(1)))

  map.new()
  |> map.insert(ok_atom, 3)
  |> map.insert(error_atom, 1)
  |> dynamic.from
  |> dynamic.field(ok_atom)
  |> should.equal(Ok(dynamic.from(3)))

  map.new()
  |> dynamic.from
  |> dynamic.field(ok_atom)
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.field(ok_atom)
  |> should.be_error

  []
  |> dynamic.from
  |> dynamic.field([])
  |> should.be_error
}

pub fn element_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let ok_one_tuple = tuple(ok_atom, 1)

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(0)
  |> should.equal(Ok(dynamic.from(ok_atom)))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(1)
  |> should.equal(Ok(dynamic.from(1)))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(2)
  |> should.be_error

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(-1)
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.element(0)
  |> should.be_error

  map.new()
  |> map.insert(1, ok_atom)
  |> dynamic.from
  |> dynamic.element(0)
  |> should.be_error
}

pub fn tuple2_test() {
  tuple(1, 2)
  |> dynamic.from
  |> dynamic.tuple2
  |> should.equal(Ok(tuple(dynamic.from(1), dynamic.from(2))))

  tuple(1, "")
  |> dynamic.from
  |> dynamic.tuple2
  |> should.equal(Ok(tuple(dynamic.from(1), dynamic.from(""))))

  tuple(1, 2, 3)
  |> dynamic.from
  |> dynamic.tuple2
  |> should.equal(Error("Expected a 2 element tuple, got a 3 element tuple"))

  1
  |> dynamic.from
  |> dynamic.tuple2
  |> should.equal(Error("Expected a 2 element tuple, got an int"))
}

pub fn typed_tuple2_test() {
  tuple(1, 2)
  |> dynamic.from
  |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
  |> should.equal(Ok(tuple(1, 2)))

  tuple(1, "")
  |> dynamic.from
  |> dynamic.typed_tuple2(dynamic.int, dynamic.string)
  |> should.equal(Ok(tuple(1, "")))

  tuple(1, "")
  |> dynamic.from
  |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error("Expected an int, got a binary"))

  tuple(1, 2, 3)
  |> dynamic.from
  |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error("Expected a 2 element tuple, got a 3 element tuple"))

  1
  |> dynamic.from
  |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error("Expected a 2 element tuple, got an int"))
}

pub fn map_test() {
  map.new()
  |> dynamic.from
  |> dynamic.map
  |> should.equal(Ok(map.new()))

  1
  |> dynamic.from
  |> dynamic.map
  |> should.equal(Error("Expected a map, got an int"))
}

pub fn list_test() {
  []
  |> dynamic.from
  |> dynamic.list
  |> should.equal(Ok([]))

  [1, 2]
  |> dynamic.from
  |> dynamic.list
  |> should.equal(Ok([dynamic.from(1), dynamic.from(2)]))

  [dynamic.from(1), dynamic.from(2.0)]
  |> dynamic.from
  |> dynamic.list
  |> should.equal(Ok([dynamic.from(1), dynamic.from(2.0)]))

  1
  |> dynamic.from
  |> dynamic.list
  |> should.equal(Error("Expected a list, got an int"))
}

pub fn result_test() {
  Ok(1)
  |> dynamic.from
  |> dynamic.result
  |> should.equal(Ok(Ok(dynamic.from(1))))

  Error("error")
  |> dynamic.from
  |> dynamic.result
  |> should.equal(Ok(Error(dynamic.from("error"))))

  1
  |> dynamic.from
  |> dynamic.result
  |> should.equal(Error("Expected a 2 element tuple, got an int"))

  let tag = atom.create_from_string("bad")

  tuple(tag, "value")
  |> dynamic.from
  |> dynamic.result
  |> should.equal(Error("Expected a tag of \"ok\" or \"error\", got \"bad\""))
}

pub fn typed_result_test() {
  Ok(1)
  |> dynamic.from
  |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Ok(Ok(1)))

  Error("error")
  |> dynamic.from
  |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Ok(Error("error")))

  Ok("1")
  |> dynamic.from
  |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Error("Expected an int, got a binary"))

  Error(1)
  |> dynamic.from
  |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Error("Expected a bit_string, got an int"))

  1
  |> dynamic.from
  |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Error("Expected a 2 element tuple, got an int"))
}

import gleam/dynamic.{DecodeError}
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import gleam/should

pub fn bit_array_test() {
  <<>>
  |> dynamic.from
  |> dynamic.bit_array
  |> should.equal(Ok(<<>>))

  <<"Hello":utf8>>
  |> dynamic.from
  |> dynamic.bit_array
  |> should.equal(Ok(<<"Hello":utf8>>))

  1
  |> dynamic.from
  |> dynamic.bit_array
  |> should.equal(
    Error([DecodeError(expected: "BitArray", found: "Int", path: [])]),
  )

  []
  |> dynamic.from
  |> dynamic.bit_array
  |> should.equal(
    Error([DecodeError(expected: "BitArray", found: "List", path: [])]),
  )
}

@target(erlang)
pub fn bit_array_erlang_test() {
  <<65_535:16>>
  |> dynamic.from
  |> dynamic.bit_array
  |> should.equal(Ok(<<65_535:16>>))
}

@target(javascript)
@external(javascript, "../gleam_stdlib_test_ffi.mjs", "uint8array")
fn uint8array(a: List(Int)) -> dynamic.Dynamic

@target(javascript)
pub fn bit_array_erlang_test() {
  [1, 1, 2, 3, 5, 8]
  |> uint8array
  |> dynamic.bit_array
  |> should.equal(Ok(<<1, 1, 2, 3, 5, 8>>))
}

@target(erlang)
pub type MyAtom {
  ThisIsAnAtom
}

@target(erlang)
pub fn map_from_atom_test() {
  ThisIsAnAtom
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Atom", path: [])]),
  )
}

@target(javascript)
@external(javascript, "../gleam_stdlib_test_ffi.mjs", "get_null")
fn get_null() -> dynamic.Dynamic

@target(javascript)
pub fn map_from_null_test() {
  get_null()
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Null", path: [])]),
  )
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

  1
  |> dynamic.from
  |> dynamic.string
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: [])]),
  )

  []
  |> dynamic.from
  |> dynamic.string
  |> should.equal(
    Error([DecodeError(expected: "String", found: "List", path: [])]),
  )
}

@target(erlang)
pub fn string_non_utf8_test() {
  <<65_535:16>>
  |> dynamic.from
  |> dynamic.string
  |> should.equal(
    Error([DecodeError(expected: "String", found: "BitArray", path: [])]),
  )
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

  []
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Error([DecodeError(expected: "Int", found: "List", path: [])]),
  )
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

  []
  |> dynamic.from
  |> dynamic.float
  |> should.equal(
    Error([DecodeError(expected: "Float", found: "List", path: [])]),
  )
}

@target(erlang)
pub fn float_on_js_is_also_int_test() {
  1
  |> dynamic.from
  |> dynamic.float
  |> should.equal(
    Error([DecodeError(expected: "Float", found: "Int", path: [])]),
  )

  1.0
  |> dynamic.from
  |> dynamic.int
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "Float", path: [])]),
  )
}

@target(javascript)
pub fn float_on_js_is_also_int_test() {
  1
  |> dynamic.from
  |> dynamic.float
  |> should.equal(Ok(1.0))

  1.0
  |> dynamic.from
  |> dynamic.int
  |> should.equal(Ok(1))
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
  |> should.equal(Error([DecodeError(expected: "Bool", found: "Int", path: [])]),
  )

  1.5
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(
    Error([DecodeError(expected: "Bool", found: "Float", path: [])]),
  )

  []
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(
    Error([DecodeError(expected: "Bool", found: "List", path: [])]),
  )
}

pub fn list_test() {
  []
  |> dynamic.from
  |> dynamic.list(dynamic.string)
  |> should.equal(Ok([]))

  []
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(Ok([]))

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> dynamic.from
  |> dynamic.list(dynamic.list(dynamic.int))
  |> should.equal(Ok([[1], [2], [3]]))

  1
  |> dynamic.from
  |> dynamic.list(dynamic.string)
  |> should.equal(Error([DecodeError(expected: "List", found: "Int", path: [])]),
  )

  1.1
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "List", found: "Float", path: [])]),
  )

  [""]
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["*"])]),
  )

  [dynamic.from(1), dynamic.from("not an int")]
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["*"])]),
  )
}

pub fn optional_test() {
  1
  |> dynamic.from
  |> dynamic.optional(dynamic.int)
  |> should.equal(Ok(Some(1)))

  option.None
  |> dynamic.from
  |> dynamic.optional(dynamic.int)
  |> should.equal(Ok(None))

  Nil
  |> dynamic.from
  |> dynamic.optional(dynamic.int)
  |> should.equal(Ok(None))

  1
  |> dynamic.from
  |> dynamic.optional(dynamic.string)
  |> should.be_error
}

@target(javascript)
pub fn javascript_object_field_test() {
  Ok(123)
  |> dynamic.from
  |> dynamic.field("0", dynamic.int)
  |> should.equal(Ok(123))

  Ok(123)
  |> dynamic.from
  |> dynamic.field(0, dynamic.int)
  |> should.equal(Ok(123))

  Ok(123)
  |> dynamic.from
  |> dynamic.field("Nope", dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Map", found: "Result", path: [])]),
  )
}

pub fn field_test() {
  dict.new()
  |> dict.insert("ok", 1)
  |> dynamic.from
  |> dynamic.field(named: "ok", of: dynamic.int)
  |> should.equal(Ok(1))

  dict.new()
  |> dict.insert("ok", 1.0)
  |> dynamic.from
  |> dynamic.field(named: "ok", of: dynamic.float)
  |> should.equal(Ok(1.0))

  dict.new()
  |> dict.insert("ok", 3)
  |> dict.insert("error", 1)
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.equal(Ok(3))

  dict.new()
  |> dict.insert("ok", 3)
  |> dynamic.from
  |> dynamic.field("ok", dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: ["ok"])]),
  )

  dict.new()
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "field", found: "nothing", path: ["ok"])]),
  )

  1
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Int", path: [])]))

  []
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "List", path: [])]),
  )

  dict.new()
  |> dict.insert("ok", 1)
  |> dynamic.from
  |> dynamic.field("ok", dynamic.field("not_a_field", dynamic.int))
  |> should.equal(
    Error([DecodeError(expected: "Map", found: "Int", path: ["ok"])]),
  )
}

pub fn optional_field_test() {
  dict.new()
  |> dict.insert("ok", 1)
  |> dynamic.from
  |> dynamic.optional_field(named: "ok", of: dynamic.int)
  |> should.equal(Ok(Some(1)))

  dict.new()
  |> dict.insert("ok", 1.0)
  |> dynamic.from
  |> dynamic.optional_field(named: "ok", of: dynamic.float)
  |> should.equal(Ok(Some(1.0)))

  dict.new()
  |> dict.insert("ok", 3)
  |> dict.insert("error", 1)
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Ok(Some(3)))

  dict.new()
  |> dict.insert("ok", 3)
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: ["ok"])]),
  )

  dict.new()
  |> dict.insert("ok", None)
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Ok(None))

  dict.new()
  |> dict.insert("ok", Nil)
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Ok(None))

  dict.new()
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Ok(None))

  1
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Int", path: [])]))

  []
  |> dynamic.from
  |> dynamic.optional_field("ok", dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "List", path: [])]),
  )
}

pub fn element_test() {
  let ok_one_tuple = #("ok", 1)

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(0, dynamic.string)
  |> should.equal(Ok("ok"))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(1, dynamic.int)
  |> should.equal(Ok(1))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(1, dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: ["1"])]),
  )

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(2, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of at least 3 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(-1, dynamic.int)
  |> should.equal(Ok(1))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(-3, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of at least 3 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.element(-3, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Tuple", found: "Int", path: [])]),
  )

  1
  |> dynamic.from
  |> dynamic.element(0, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Tuple", found: "Int", path: [])]),
  )

  dict.new()
  |> dict.insert(1, "ok")
  |> dynamic.from
  |> dynamic.element(0, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Tuple", found: "Map", path: [])]),
  )
}

pub fn tuple2_test() {
  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2)))

  #(1, "")
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.string)
  |> should.equal(Ok(#(1, "")))

  #(1, "")
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["1"])]),
  )

  #(1.2, "")
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
    ]),
  )

  #(1, 2, 3)
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of 2 elements",
        found: "Tuple of 3 elements",
      ),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(path: [], expected: "Tuple of 2 elements", found: "Int")]),
  )

  [1, 2]
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2)))

  [dynamic.from(1), dynamic.from("a")]
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.string)
  |> should.equal(Ok(#(1, "a")))

  ["", ""]
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "String", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
    ]),
  )

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 2 elements", found: "List"),
    ]),
  )

  []
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 2 elements", found: "List"),
    ]),
  )
}

pub fn tuple3_test() {
  #(1, 2, 3)
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2, 3)))

  #(1, "", 3.0)
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.string, dynamic.float)
  |> should.equal(Ok(#(1, "", 3.0)))

  [1, 2, 3]
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2, 3)))

  [dynamic.from(1), dynamic.from("a"), dynamic.from(3.0)]
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.string, dynamic.float)
  |> should.equal(Ok(#(1, "a", 3.0)))

  #(1, 2, "")
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["2"])]),
  )

  #(1.2, "", "")
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
    ]),
  )

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of 3 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  ["", "", ""]
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "String", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
    ]),
  )

  [1, 2]
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 3 elements", found: "List"),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(path: [], expected: "Tuple of 3 elements", found: "Int")]),
  )
}

pub fn tuple4_test() {
  #(1, 2, 3, 4)
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2, 3, 4)))

  #(1, "", 3.0, 4)
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.string, dynamic.float, dynamic.int)
  |> should.equal(Ok(#(1, "", 3.0, 4)))

  [1, 2, 3, 4]
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Ok(#(1, 2, 3, 4)))

  [dynamic.from(1), dynamic.from("a"), dynamic.from(3.0), dynamic.from(4.0)]
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.string, dynamic.float, dynamic.float)
  |> should.equal(Ok(#(1, "a", 3.0, 4.0)))

  #(1, 2, 3, "")
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["3"])]),
  )

  #(1.2, "", "", "")
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
    ]),
  )

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of 4 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  ["", "", "", ""]
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "String", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
    ]),
  )

  [1, 2]
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 4 elements", found: "List"),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(path: [], expected: "Tuple of 4 elements", found: "Int")]),
  )
}

pub fn tuple5_test() {
  #(1, 2, 3, 4, 5)
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, 2, 3, 4, 5)))

  #(1, "", 3.0, 4, 5)
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.string,
    dynamic.float,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, "", 3.0, 4, 5)))

  [1, 2, 3, 4, 5]
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, 2, 3, 4, 5)))

  [
    dynamic.from(1),
    dynamic.from("a"),
    dynamic.from(3.0),
    dynamic.from(4.0),
    dynamic.from(True),
  ]
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.string,
    dynamic.float,
    dynamic.float,
    dynamic.bool,
  )
  |> should.equal(Ok(#(1, "a", 3.0, 4.0, True)))

  #(1, 2, 3, 4, "")
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["4"])]),
  )

  #(1.2, "", "", "", "")
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
      DecodeError(expected: "Int", found: "String", path: ["4"]),
    ]),
  )

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of 5 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  ["", "", "", "", ""]
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "String", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
      DecodeError(expected: "Int", found: "String", path: ["4"]),
    ]),
  )

  [1, 2]
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 5 elements", found: "List"),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([DecodeError(path: [], expected: "Tuple of 5 elements", found: "Int")]),
  )
}

pub fn tuple6_test() {
  #(1, 2, 3, 4, 5, 6)
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, 2, 3, 4, 5, 6)))

  #(1, "", 3.0, 4, 5, 6)
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.string,
    dynamic.float,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, "", 3.0, 4, 5, 6)))

  [1, 2, 3, 4, 5, 6]
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Ok(#(1, 2, 3, 4, 5, 6)))

  [
    dynamic.from(1),
    dynamic.from("a"),
    dynamic.from(3.0),
    dynamic.from(4.0),
    dynamic.from(True),
    dynamic.from(6.0),
  ]
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.string,
    dynamic.float,
    dynamic.float,
    dynamic.bool,
    dynamic.float,
  )
  |> should.equal(Ok(#(1, "a", 3.0, 4.0, True, 6.0)))

  #(1, 2, 3, 4, 5, "")
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["5"])]),
  )

  #(1.2, "", "", "", "", "")
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
      DecodeError(expected: "Int", found: "String", path: ["4"]),
      DecodeError(expected: "Int", found: "String", path: ["5"]),
    ]),
  )

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(
        path: [],
        expected: "Tuple of 6 elements",
        found: "Tuple of 2 elements",
      ),
    ]),
  )

  ["", "", "", "", "", ""]
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "String", path: ["0"]),
      DecodeError(expected: "Int", found: "String", path: ["1"]),
      DecodeError(expected: "Int", found: "String", path: ["2"]),
      DecodeError(expected: "Int", found: "String", path: ["3"]),
      DecodeError(expected: "Int", found: "String", path: ["4"]),
      DecodeError(expected: "Int", found: "String", path: ["5"]),
    ]),
  )

  [1, 2]
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([
      DecodeError(path: [], expected: "Tuple of 6 elements", found: "List"),
    ]),
  )

  1
  |> dynamic.from
  |> dynamic.tuple6(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(
    Error([DecodeError(path: [], expected: "Tuple of 6 elements", found: "Int")]),
  )
}

pub fn nested_tuples_test() {
  #(1, #(2, #("3", 4)))
  |> dynamic.from
  |> dynamic.tuple2(
    dynamic.int,
    dynamic.tuple2(dynamic.int, dynamic.tuple2(dynamic.int, dynamic.int)),
  )
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["1", "1", "0"])]),
  )
}

pub fn map_test() {
  dict.new()
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Ok(dict.new()))

  dict.from_list([#("a", 1), #("b", 2)])
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Ok(dict.from_list([#("a", 1), #("b", 2)])))

  dict.from_list([#("a", 1), #("b", 2)])
  |> dynamic.from
  |> dynamic.map(dynamic.int, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["keys"])]),
  )

  dict.from_list([#("a", 1), #("b", 2)])
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: ["values"])]),
  )

  1
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Int", path: [])]))

  #()
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Map", found: "Tuple of 0 elements", path: [])]),
  )

  fn() { Nil }
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(
    Error([DecodeError(expected: "Map", found: "Function", path: [])]),
  )

  Nil
  |> dynamic.from
  |> dynamic.map(dynamic.string, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Map", found: "Nil", path: [])]))
}

pub fn shallow_list_test() {
  []
  |> dynamic.from
  |> dynamic.shallow_list
  |> should.equal(Ok([]))

  [1, 2]
  |> dynamic.from
  |> dynamic.shallow_list
  |> should.equal(Ok([dynamic.from(1), dynamic.from(2)]))

  [dynamic.from(1), dynamic.from(2.0)]
  |> dynamic.from
  |> dynamic.shallow_list
  |> should.equal(Ok([dynamic.from(1), dynamic.from(2.0)]))

  1
  |> dynamic.from
  |> dynamic.shallow_list
  |> should.equal(Error([DecodeError(expected: "List", found: "Int", path: [])]),
  )
}

@target(javascript)
pub fn array_on_js_is_also_list_test() {
  #()
  |> dynamic.from
  |> dynamic.shallow_list
  |> should.equal(Ok([]))

  #(1, 2)
  |> dynamic.from
  |> dynamic.list(of: dynamic.int)
  |> should.equal(Ok([1, 2]))
}

pub fn result_test() {
  Ok(1)
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Ok(Ok(1)))

  Error("error")
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Ok(Error("error")))

  Ok("1")
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "String", path: ["ok"])]),
  )

  Error(1)
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "String", found: "Int", path: ["error"])]),
  )

  1
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(
    Error([DecodeError(expected: "Result", found: "Int", path: [])]),
  )
}

pub fn any_test() {
  let decoder =
    dynamic.any([
      fn(x) { result.map(dynamic.int(x), fn(_) { "int" }) },
      fn(x) { result.map(dynamic.float(x), fn(_) { "float" }) },
    ])

  1
  |> dynamic.from
  |> decoder
  |> should.equal(Ok("int"))

  1.1
  |> dynamic.from
  |> decoder
  |> should.equal(Ok("float"))

  ""
  |> dynamic.from
  |> decoder
  |> should.equal(Error([DecodeError("another type", "String", path: [])]))
}

type One(a) {
  One(a)
}

pub fn decode1_test() {
  let decoder = dynamic.decode1(One, dynamic.element(0, dynamic.int))

  #(1)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(One(1)))

  #(1.3)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([DecodeError(expected: "Int", found: "Float", path: ["0"])]),
  )
}

type Two(a, b) {
  Two(a, b)
}

pub fn decode2_test() {
  let decoder =
    dynamic.decode2(
      Two,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
    )

  #(1, "2")
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Two(1, "2")))

  #(1.3, 2)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Int", path: ["1"]),
    ]),
  )
}

type Three(a, b, c) {
  Three(a, b, c)
}

pub fn decode3_test() {
  let decoder =
    dynamic.decode3(
      Three,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
    )

  #(1, "2", 3)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Three(1, "2", 3)))

  #(1.3, 2.1, 3)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Four(a, b, c, d) {
  Four(a, b, c, d)
}

pub fn decode4_test() {
  let decoder =
    dynamic.decode4(
      Four,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
    )

  #(1, "2", 3, 4)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Four(1, "2", 3, 4)))

  #(1.3, 2.1, 3, 4)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Five(a, b, c, d, e) {
  Five(a, b, c, d, e)
}

pub fn decode5_test() {
  let decoder =
    dynamic.decode5(
      Five,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
      dynamic.element(4, dynamic.int),
    )

  #(1, "2", 3, 4, 5)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Five(1, "2", 3, 4, 5)))

  #(1.3, 2.1, 3, 4, 5)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Six(a, b, c, d, e, f) {
  Six(a, b, c, d, e, f)
}

pub fn decode6_test() {
  let decoder =
    dynamic.decode6(
      Six,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
      dynamic.element(4, dynamic.int),
      dynamic.element(5, dynamic.int),
    )

  #(1, "2", 3, 4, 5, 6)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Six(1, "2", 3, 4, 5, 6)))

  #(1.3, 2.1, 3, 4, 5, 6)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Seven(a, b, c, d, e, f, g) {
  Seven(a, b, c, d, e, f, g)
}

pub fn decode7_test() {
  let decoder =
    dynamic.decode7(
      Seven,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
      dynamic.element(4, dynamic.int),
      dynamic.element(5, dynamic.int),
      dynamic.element(6, dynamic.int),
    )

  #(1, "2", 3, 4, 5, 6, 7)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Seven(1, "2", 3, 4, 5, 6, 7)))

  #(1.3, 2.1, 3, 4, 5, 6, 7)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Eight(a, b, c, d, e, f, g, h) {
  Eight(a, b, c, d, e, f, g, h)
}

pub fn decode8_test() {
  let decoder =
    dynamic.decode8(
      Eight,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
      dynamic.element(4, dynamic.int),
      dynamic.element(5, dynamic.int),
      dynamic.element(6, dynamic.int),
      dynamic.element(7, dynamic.int),
    )

  #(1, "2", 3, 4, 5, 6, 7, 8)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Eight(1, "2", 3, 4, 5, 6, 7, 8)))

  #(1.3, 2.1, 3, 4, 5, 6, 7, 8)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

type Nine(a, b, c, d, e, f, g, h, i) {
  Nine(a, b, c, d, e, f, g, h, i)
}

pub fn decode9_test() {
  let decoder =
    dynamic.decode9(
      Nine,
      dynamic.element(0, dynamic.int),
      dynamic.element(1, dynamic.string),
      dynamic.element(2, dynamic.int),
      dynamic.element(3, dynamic.int),
      dynamic.element(4, dynamic.int),
      dynamic.element(5, dynamic.int),
      dynamic.element(6, dynamic.int),
      dynamic.element(7, dynamic.int),
      dynamic.element(8, dynamic.int),
    )

  #(1, "2", 3, 4, 5, 6, 7, 8, 9)
  |> dynamic.from
  |> decoder
  |> should.equal(Ok(Nine(1, "2", 3, 4, 5, 6, 7, 8, 9)))

  #(1.3, 2.1, 3, 4, 5, 6, 7, 8, 9)
  |> dynamic.from
  |> decoder
  |> should.equal(
    Error([
      DecodeError(expected: "Int", found: "Float", path: ["0"]),
      DecodeError(expected: "String", found: "Float", path: ["1"]),
    ]),
  )
}

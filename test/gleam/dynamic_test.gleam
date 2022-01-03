import gleam/should
import gleam/dynamic.{DecodeError}
import gleam/bit_string
import gleam/result
import gleam/map
import gleam/option.{None, Some}

pub fn bit_string_test() {
  <<>>
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Ok(<<>>))

  <<"Hello":utf8>>
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Ok(<<"Hello":utf8>>))

  1
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Error([
    DecodeError(expected: "BitString", found: "Int", path: []),
  ]))

  []
  |> dynamic.from
  |> dynamic.bit_string
  |> should.equal(Error([
    DecodeError(expected: "BitString", found: "List", path: []),
  ]))
}

if erlang {
  pub fn bit_string_erlang_test() {
    <<65535:16>>
    |> dynamic.from
    |> dynamic.bit_string
    |> should.equal(Ok(<<65535:16>>))
  }
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
  |> should.equal(Error([
    DecodeError(expected: "String", found: "Int", path: []),
  ]))

  []
  |> dynamic.from
  |> dynamic.string
  |> should.equal(Error([
    DecodeError(expected: "String", found: "List", path: []),
  ]))
}

if erlang {
  pub fn string_non_utf8_test() {
    <<65535:16>>
    |> dynamic.from
    |> dynamic.string
    |> should.equal(Error([
      DecodeError(expected: "String", found: "BitString", path: []),
    ]))
  }
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
  |> should.equal(Error([DecodeError(expected: "Int", found: "List", path: [])]))
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
  |> should.equal(Error([
    DecodeError(expected: "Float", found: "List", path: []),
  ]))
}

if erlang {
  pub fn float_on_js_is_also_int_test() {
    1
    |> dynamic.from
    |> dynamic.float
    |> should.equal(Error([
      DecodeError(expected: "Float", found: "Int", path: []),
    ]))

    1.0
    |> dynamic.from
    |> dynamic.int
    |> should.equal(Error([
      DecodeError(expected: "Int", found: "Float", path: []),
    ]))
  }
}

if javascript {
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
  |> should.equal(Error([DecodeError(expected: "Bool", found: "Int", path: [])]))

  1.5
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(Error([
    DecodeError(expected: "Bool", found: "Float", path: []),
  ]))

  []
  |> dynamic.from
  |> dynamic.bool
  |> should.equal(Error([DecodeError(expected: "Bool", found: "List", path: [])]))
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
  |> dynamic.list(dynamic.list(_, dynamic.int))
  |> should.equal(Ok([[1], [2], [3]]))

  1
  |> dynamic.from
  |> dynamic.list(dynamic.string)
  |> should.equal(Error([DecodeError(expected: "List", found: "Int", path: [])]))

  1.1
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "List", found: "Float", path: []),
  ]))

  [""]
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["*"]),
  ]))

  [dynamic.from(1), dynamic.from("not an int")]
  |> dynamic.from
  |> dynamic.list(dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["*"]),
  ]))
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

if javascript {
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
    |> should.equal(Error([
      DecodeError(expected: "object", found: "Result", path: []),
    ]))
  }
}

pub fn field_test() {
  map.new()
  |> map.insert("ok", 1)
  |> dynamic.from
  |> dynamic.field(named: "ok", of: dynamic.int)
  |> should.equal(Ok(1))

  map.new()
  |> map.insert("ok", 1.0)
  |> dynamic.from
  |> dynamic.field(named: "ok", of: dynamic.float)
  |> should.equal(Ok(1.0))

  map.new()
  |> map.insert("ok", 3)
  |> map.insert("error", 1)
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.equal(Ok(3))

  map.new()
  |> map.insert("ok", 3)
  |> dynamic.from
  |> dynamic.field("ok", dynamic.string)
  |> should.be_error

  map.new()
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.be_error

  1
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.be_error

  []
  |> dynamic.from
  |> dynamic.field("ok", dynamic.int)
  |> should.be_error
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
  |> should.equal(Error([
    DecodeError(expected: "String", found: "Int", path: ["1"]),
  ]))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(2, dynamic.int)
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of at least 3 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(-1, dynamic.int)
  |> should.equal(Ok(1))

  ok_one_tuple
  |> dynamic.from
  |> dynamic.element(-3, dynamic.int)
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of at least 3 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

  1
  |> dynamic.from
  |> dynamic.element(-3, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Tuple", found: "Int", path: [])]))

  1
  |> dynamic.from
  |> dynamic.element(0, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Tuple", found: "Int", path: [])]))

  map.new()
  |> map.insert(1, "ok")
  |> dynamic.from
  |> dynamic.element(0, dynamic.int)
  |> should.equal(Error([DecodeError(expected: "Tuple", found: "Map", path: [])]))
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
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["1"]),
  ]))

  #(1.2, "")
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "Float", path: ["0"]),
    DecodeError(expected: "Int", found: "String", path: ["1"]),
  ]))

  #(1, 2, 3)
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of 2 elements",
      found: "Tuple of 3 elements",
    ),
  ]))

  1
  |> dynamic.from
  |> dynamic.tuple2(dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(path: [], expected: "Tuple of 2 elements", found: "Int"),
  ]))
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

  #(1, 2, "")
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["2"]),
  ]))

  #(1.2, "", "")
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "Float", path: ["0"]),
    DecodeError(expected: "Int", found: "String", path: ["1"]),
    DecodeError(expected: "Int", found: "String", path: ["2"]),
  ]))

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of 3 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

  1
  |> dynamic.from
  |> dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(path: [], expected: "Tuple of 3 elements", found: "Int"),
  ]))
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

  #(1, 2, 3, "")
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["3"]),
  ]))

  #(1.2, "", "", "")
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "Float", path: ["0"]),
    DecodeError(expected: "Int", found: "String", path: ["1"]),
    DecodeError(expected: "Int", found: "String", path: ["2"]),
    DecodeError(expected: "Int", found: "String", path: ["3"]),
  ]))

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of 4 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

  1
  |> dynamic.from
  |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
  |> should.equal(Error([
    DecodeError(path: [], expected: "Tuple of 4 elements", found: "Int"),
  ]))
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

  #(1, 2, 3, 4, "")
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["4"]),
  ]))

  #(1.2, "", "", "", "")
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "Float", path: ["0"]),
    DecodeError(expected: "Int", found: "String", path: ["1"]),
    DecodeError(expected: "Int", found: "String", path: ["2"]),
    DecodeError(expected: "Int", found: "String", path: ["3"]),
    DecodeError(expected: "Int", found: "String", path: ["4"]),
  ]))

  #(1, 2)
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of 5 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

  1
  |> dynamic.from
  |> dynamic.tuple5(
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
    dynamic.int,
  )
  |> should.equal(Error([
    DecodeError(path: [], expected: "Tuple of 5 elements", found: "Int"),
  ]))
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
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: ["5"]),
  ]))

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
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "Float", path: ["0"]),
    DecodeError(expected: "Int", found: "String", path: ["1"]),
    DecodeError(expected: "Int", found: "String", path: ["2"]),
    DecodeError(expected: "Int", found: "String", path: ["3"]),
    DecodeError(expected: "Int", found: "String", path: ["4"]),
    DecodeError(expected: "Int", found: "String", path: ["5"]),
  ]))

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
  |> should.equal(Error([
    DecodeError(
      path: [],
      expected: "Tuple of 6 elements",
      found: "Tuple of 2 elements",
    ),
  ]))

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
  |> should.equal(Error([
    DecodeError(path: [], expected: "Tuple of 6 elements", found: "Int"),
  ]))
}

pub fn map_test() {
  map.new()
  |> dynamic.from
  |> dynamic.map
  |> should.equal(Ok(map.new()))

  1
  |> dynamic.from
  |> dynamic.map
  |> should.equal(Error([DecodeError(expected: "Map", found: "Int", path: [])]))
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
  |> should.equal(Error([DecodeError(expected: "List", found: "Int", path: [])]))
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
  |> should.equal(Error([
    DecodeError(expected: "Int", found: "String", path: []),
  ]))

  Error(1)
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Error([
    DecodeError(expected: "String", found: "Int", path: []),
  ]))

  1
  |> dynamic.from
  |> dynamic.result(ok: dynamic.int, error: dynamic.string)
  |> should.equal(Error([
    DecodeError(expected: "Result", found: "Int", path: []),
  ]))
}

pub fn any_test() {
  let decoder = dynamic.any(
    _,
    [
      fn(x) { result.map(dynamic.int(x), fn(_) { "int" }) },
      fn(x) { result.map(dynamic.float(x), fn(_) { "float" }) },
    ],
  )

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

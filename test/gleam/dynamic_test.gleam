if erlang {
  import gleam/bit_string
  import gleam/dynamic.{DecodeError}
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
    |> should.equal(Error(DecodeError(expected: "bit_string", got: "int")))

    []
    |> dynamic.from
    |> dynamic.bit_string
    |> should.equal(Error(DecodeError(expected: "bit_string", got: "list")))
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
    |> should.equal(Error(DecodeError(expected: "string", got: "bit_string")))

    1
    |> dynamic.from
    |> dynamic.string
    |> should.equal(Error(DecodeError(expected: "bit_string", got: "int")))

    []
    |> dynamic.from
    |> dynamic.string
    |> should.equal(Error(DecodeError(expected: "bit_string", got: "list")))
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
    |> should.equal(Error(DecodeError(expected: "int", got: "float")))

    []
    |> dynamic.from
    |> dynamic.int
    |> should.equal(Error(DecodeError(expected: "int", got: "list")))
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
    |> should.equal(Error(DecodeError(expected: "float", got: "int")))

    []
    |> dynamic.from
    |> dynamic.float
    |> should.equal(Error(DecodeError(expected: "float", got: "list")))
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
    |> should.equal(Error(DecodeError(expected: "bool", got: "int")))

    []
    |> dynamic.from
    |> dynamic.bool
    |> should.equal(Error(DecodeError(expected: "bool", got: "list")))
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

  pub fn field_test() {
    map.new()
    |> map.insert("ok", 1)
    |> dynamic.from
    |> dynamic.field("ok")
    |> should.equal(Ok(dynamic.from(1)))

    map.new()
    |> map.insert("ok", 3)
    |> map.insert("error", 1)
    |> dynamic.from
    |> dynamic.field("ok")
    |> should.equal(Ok(dynamic.from(3)))

    map.new()
    |> dynamic.from
    |> dynamic.field("ok")
    |> should.be_error

    1
    |> dynamic.from
    |> dynamic.field("ok")
    |> should.be_error

    []
    |> dynamic.from
    |> dynamic.field([])
    |> should.be_error
  }

  pub fn element_test() {
    let ok_one_tuple = #("ok", 1)

    ok_one_tuple
    |> dynamic.from
    |> dynamic.element(0)
    |> should.equal(Ok(dynamic.from("ok")))

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
    |> map.insert(1, "ok")
    |> dynamic.from
    |> dynamic.element(0)
    |> should.be_error
  }

  pub fn tuple2_test() {
    #(1, 2)
    |> dynamic.from
    |> dynamic.tuple2
    |> should.equal(Ok(#(dynamic.from(1), dynamic.from(2))))

    #(1, "")
    |> dynamic.from
    |> dynamic.tuple2
    |> should.equal(Ok(#(dynamic.from(1), dynamic.from(""))))

    #(1, 2, 3)
    |> dynamic.from
    |> dynamic.tuple2
    |> should.equal(Error(DecodeError(
      expected: "2 element tuple",
      got: "3 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.tuple2
    |> should.equal(Error(DecodeError(expected: "2 element tuple", got: "int")))
  }

  pub fn typed_tuple2_test() {
    #(1, 2)
    |> dynamic.from
    |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
    |> should.equal(Ok(#(1, 2)))

    #(1, "")
    |> dynamic.from
    |> dynamic.typed_tuple2(dynamic.int, dynamic.string)
    |> should.equal(Ok(#(1, "")))

    #(1, "")
    |> dynamic.from
    |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    #(1, 2, 3)
    |> dynamic.from
    |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(
      expected: "2 element tuple",
      got: "3 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.typed_tuple2(dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "2 element tuple", got: "int")))
  }

  pub fn tuple3_test() {
    #(1, 2, 3)
    |> dynamic.from
    |> dynamic.tuple3
    |> should.equal(Ok(#(dynamic.from(1), dynamic.from(2), dynamic.from(3))))

    #(1, "", 3.0)
    |> dynamic.from
    |> dynamic.tuple3
    |> should.equal(Ok(#(dynamic.from(1), dynamic.from(""), dynamic.from(3.0))))

    #(1, 2)
    |> dynamic.from
    |> dynamic.tuple3
    |> should.equal(Error(DecodeError(
      expected: "3 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.tuple3
    |> should.equal(Error(DecodeError(expected: "3 element tuple", got: "int")))
  }

  pub fn typed_tuple3_test() {
    #(1, 2, 3)
    |> dynamic.from
    |> dynamic.typed_tuple3(dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Ok(#(1, 2, 3)))

    #(1, "", 3.0)
    |> dynamic.from
    |> dynamic.typed_tuple3(dynamic.int, dynamic.string, dynamic.float)
    |> should.equal(Ok(#(1, "", 3.0)))

    #(1, 2, "")
    |> dynamic.from
    |> dynamic.typed_tuple3(dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    #(1, 2)
    |> dynamic.from
    |> dynamic.typed_tuple3(dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(
      expected: "3 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.typed_tuple3(dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "3 element tuple", got: "int")))
  }

  pub fn tuple4_test() {
    #(1, 2, 3, 4)
    |> dynamic.from
    |> dynamic.tuple4
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(2),
      dynamic.from(3),
      dynamic.from(4),
    )))

    #(1, "", 3.0, 4)
    |> dynamic.from
    |> dynamic.tuple4
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(""),
      dynamic.from(3.0),
      dynamic.from(4),
    )))

    #(1, 2)
    |> dynamic.from
    |> dynamic.tuple4
    |> should.equal(Error(DecodeError(
      expected: "4 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.tuple4
    |> should.equal(Error(DecodeError(expected: "4 element tuple", got: "int")))
  }

  pub fn typed_tuple4_test() {
    #(1, 2, 3, 4)
    |> dynamic.from
    |> dynamic.typed_tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Ok(#(1, 2, 3, 4)))

    #(1, "", 3.0, 4)
    |> dynamic.from
    |> dynamic.typed_tuple4(
      dynamic.int,
      dynamic.string,
      dynamic.float,
      dynamic.int,
    )
    |> should.equal(Ok(#(1, "", 3.0, 4)))

    #(1, 2, 3, "")
    |> dynamic.from
    |> dynamic.typed_tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    #(1, 2)
    |> dynamic.from
    |> dynamic.typed_tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(
      expected: "4 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.typed_tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
    |> should.equal(Error(DecodeError(expected: "4 element tuple", got: "int")))
  }

  pub fn tuple5_test() {
    #(1, 2, 3, 4, 5)
    |> dynamic.from
    |> dynamic.tuple5
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(2),
      dynamic.from(3),
      dynamic.from(4),
      dynamic.from(5),
    )))

    #(1, "", 3.0, 4, 5)
    |> dynamic.from
    |> dynamic.tuple5
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(""),
      dynamic.from(3.0),
      dynamic.from(4),
      dynamic.from(5),
    )))

    #(1, 2)
    |> dynamic.from
    |> dynamic.tuple5
    |> should.equal(Error(DecodeError(
      expected: "5 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.tuple5
    |> should.equal(Error(DecodeError(expected: "5 element tuple", got: "int")))
  }

  pub fn typed_tuple5_test() {
    #(1, 2, 3, 4, 5)
    |> dynamic.from
    |> dynamic.typed_tuple5(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Ok(#(1, 2, 3, 4, 5)))

    #(1, "", 3.0, 4, 5)
    |> dynamic.from
    |> dynamic.typed_tuple5(
      dynamic.int,
      dynamic.string,
      dynamic.float,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Ok(#(1, "", 3.0, 4, 5)))

    #(1, 2, 3, 4, "")
    |> dynamic.from
    |> dynamic.typed_tuple5(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    #(1, 2)
    |> dynamic.from
    |> dynamic.typed_tuple5(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(
      expected: "5 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.typed_tuple5(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(expected: "5 element tuple", got: "int")))
  }

  pub fn tuple6_test() {
    #(1, 2, 3, 4, 5, 6)
    |> dynamic.from
    |> dynamic.tuple6
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(2),
      dynamic.from(3),
      dynamic.from(4),
      dynamic.from(5),
      dynamic.from(6),
    )))

    #(1, "", 3.0, 4, 5, 6)
    |> dynamic.from
    |> dynamic.tuple6
    |> should.equal(Ok(#(
      dynamic.from(1),
      dynamic.from(""),
      dynamic.from(3.0),
      dynamic.from(4),
      dynamic.from(5),
      dynamic.from(6),
    )))

    #(1, 2)
    |> dynamic.from
    |> dynamic.tuple6
    |> should.equal(Error(DecodeError(
      expected: "6 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.tuple6
    |> should.equal(Error(DecodeError(expected: "6 element tuple", got: "int")))
  }

  pub fn typed_tuple6_test() {
    #(1, 2, 3, 4, 5, 6)
    |> dynamic.from
    |> dynamic.typed_tuple6(
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
    |> dynamic.typed_tuple6(
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
    |> dynamic.typed_tuple6(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    #(1, 2)
    |> dynamic.from
    |> dynamic.typed_tuple6(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(
      expected: "6 element tuple",
      got: "2 element tuple",
    )))

    1
    |> dynamic.from
    |> dynamic.typed_tuple6(
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
      dynamic.int,
    )
    |> should.equal(Error(DecodeError(expected: "6 element tuple", got: "int")))
  }

  pub fn map_test() {
    map.new()
    |> dynamic.from
    |> dynamic.map
    |> should.equal(Ok(map.new()))

    1
    |> dynamic.from
    |> dynamic.map
    |> should.equal(Error(DecodeError(expected: "map", got: "int")))
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
    |> should.equal(Error(DecodeError(expected: "list", got: "int")))
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
    |> should.equal(Error(DecodeError(expected: "result tuple", got: "int")))

    #("bad", "value")
    |> dynamic.from
    |> dynamic.result
    |> should.equal(Error(DecodeError(
      expected: "result tuple",
      got: "2 element tuple",
    )))
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
    |> should.equal(Error(DecodeError(expected: "int", got: "binary")))

    Error(1)
    |> dynamic.from
    |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
    |> should.equal(Error(DecodeError(expected: "bit_string", got: "int")))

    1
    |> dynamic.from
    |> dynamic.typed_result(ok: dynamic.int, error: dynamic.string)
    |> should.equal(Error(DecodeError(expected: "result tuple", got: "int")))
  }
}

if erlang {
  import gleam/bit_string
  import gleam/list
  import gleam/map.{Map}
  import gleam/option.{Option}
  import gleam/result
  import gleam/string_builder

  /// `Dynamic` data is data that we don't know the type of yet.
  /// We likely get data like this from interop with Erlang, or from
  /// IO with the outside world.
  pub external type Dynamic

  pub type DecodeError {
    DecodeError(expected: String, found: String)
  }

  pub type Decoder(t) =
    fn(Dynamic) -> Result(t, DecodeError)

  /// Converts any Gleam data into `Dynamic` data.
  ///
  pub external fn from(a) -> Dynamic =
    "gleam_stdlib" "identity"

  /// Unsafely casts a Dynamic value into any other type.
  ///
  /// This is an escape hatch for the type system that may be useful when wrapping
  /// native Erlang APIs. It is to be used as a last measure only!
  ///
  /// If you can avoid using this function, do!
  ///
  pub external fn unsafe_coerce(Dynamic) -> a =
    "gleam_stdlib" "identity"

  /// Checks to see whether a Dynamic value is a bit_string, and return the bit_string if
  /// it is.
  ///
  /// ## Examples
  ///
  ///    > bit_string(from("Hello")) == bit_string.from_string("Hello")
  ///    True
  ///
  ///    > bit_string(from(123))
  ///    Error("Expected a BitString, found `123`")
  ///
  pub external fn bit_string(from: Dynamic) -> Result(BitString, DecodeError) =
    "gleam_stdlib" "decode_bit_string"

  /// Checks to see whether a Dynamic value is a string, and return the string if
  /// it is.
  ///
  /// ## Examples
  ///
  ///    > string(from("Hello"))
  ///    Ok("Hello")
  ///
  ///    > string(from(123))
  ///    Error("Expected a String, found `123`")
  ///
  pub fn string(from: Dynamic) -> Result(String, DecodeError) {
    bit_string(from)
    |> result.then(fn(raw) {
      case bit_string.to_string(raw) {
        Ok(string) -> Ok(string)
        Error(Nil) ->
          Error(DecodeError(expected: "string", found: "bit_string"))
      }
    })
  }

  /// Checks to see whether a Dynamic value is an int, and return the int if it
  /// is.
  ///
  /// ## Examples
  ///
  ///    > int(from(123))
  ///    Ok(123)
  ///
  ///    > int(from("Hello"))
  ///    Error("Expected an Int, found `\"Hello World\"`")
  ///
  pub external fn int(from: Dynamic) -> Result(Int, DecodeError) =
    "gleam_stdlib" "decode_int"

  /// Checks to see whether a Dynamic value is an float, and return the float if
  /// it is.
  ///
  /// ## Examples
  ///
  ///    > float(from(2.0))
  ///    Ok(2.0)
  ///
  ///    > float(from(123))
  ///    Error("Expected a Float, found `123`")
  ///
  pub external fn float(from: Dynamic) -> Result(Float, DecodeError) =
    "gleam_stdlib" "decode_float"

  /// Checks to see whether a Dynamic value is an bool, and return the bool if
  /// it is.
  ///
  /// ## Examples
  ///
  ///    > bool(from(True))
  ///    Ok(True)
  ///
  ///    > bool(from(123))
  ///    Error("Expected a Bool, found `123`")
  ///
  pub external fn bool(from: Dynamic) -> Result(Bool, DecodeError) =
    "gleam_stdlib" "decode_bool"

  /// Checks to see whether a Dynamic value is a function that takes no arguments,
  /// and return the function if it is.
  ///
  /// ## Examples
  ///
  ///    > import gleam/result
  ///    > let f = fn() { 1 }
  ///    > thunk(from(f)) |> result.is_ok
  ///    True
  ///
  ///    > thunk(from(123))
  ///    Error("Expected a zero arity function, found `123`")
  ///
  pub external fn thunk(from: Dynamic) -> Result(fn() -> Dynamic, DecodeError) =
    "gleam_stdlib" "decode_thunk"

  /// Checks to see whether a Dynamic value is a list, and return the list if it
  /// is.
  ///
  /// If you wish to decode all the elements in the list use the `typed_list`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > list(from(["a", "b", "c"]))
  ///    Ok([from("a"), from("b"), from("c")])
  ///
  ///    > list(1)
  ///    Error("Expected an Int, found a binary")
  ///
  pub external fn list(from: Dynamic) -> Result(List(Dynamic), DecodeError) =
    "gleam_stdlib" "decode_list"

  /// Checks to see whether a Dynamic value is a result, and return the result if
  /// it is.
  ///
  /// ## Examples
  ///
  ///    > result(from(Ok(1)))
  ///    Ok(Ok(from(1)))
  ///
  ///    > result(from(Error("boom")))
  ///    Ok(Error(from("boom")))
  ///
  ///    > result(from(123))
  ///    Error("Expected a 2 element tuple, found an int")
  ///
  pub external fn result(
    Dynamic,
  ) -> Result(Result(Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_result"

  /// Checks to see whether a Dynamic value is a result of a particular type, and
  /// return the result if it is
  ///
  /// The `ok` and `error` arguments are decoders for decoding the `Ok` and
  /// `Error` values of the result.
  ///
  /// ## Examples
  ///
  ///    > typed_result(of: from(Ok(1)), ok: int, error: string)
  ///    Ok(Ok(1))
  ///
  ///    > typed_result(of: from(Error("boom")), ok: int, error: string)
  ///    Ok(Error("boom"))
  ///
  ///    > typed_result(of: from(123), ok: int, error: string)
  ///    Error("Expected a 2 element tuple, found an int")
  ///
  pub fn typed_result(
    of dynamic: Dynamic,
    ok decode_ok: Decoder(a),
    error decode_error: Decoder(e),
  ) -> Result(Result(a, e), DecodeError) {
    try inner_result = result(dynamic)

    case inner_result {
      Ok(raw) ->
        raw
        |> decode_ok
        |> result.map(Ok)
      Error(raw) ->
        raw
        |> decode_error
        |> result.map(Error)
    }
  }

  /// Checks to see whether a Dynamic value is a list of a particular type, and
  /// return the list if it is.
  ///
  /// The second argument is a decoder function used to decode the elements of
  /// the list. The list is only decoded if all elements in the list can be
  /// successfully decoded using this function.
  ///
  /// If you do not wish to decode all the elements in the list use the `list`
  /// function instead.
  ///
  /// ## Examples
  ///
  ///    > typed_list(from(["a", "b", "c"]), of: string)
  ///    Ok(["a", "b", "c"])
  ///
  ///    > typed_list(from([1, 2, 3]), of: string)
  ///    Error("Expected an Int, found a binary")
  ///
  ///    > typed_list(from("ok"), of: string)
  ///    Error("Expected a List, found a binary")
  ///
  pub fn typed_list(
    from dynamic: Dynamic,
    of decoder_type: fn(Dynamic) -> Result(inner, DecodeError),
  ) -> Result(List(inner), DecodeError) {
    dynamic
    |> list
    |> result.then(list.try_map(_, decoder_type))
  }

  /// Checks to see if a Dynamic value is a nullable version of a particular
  /// type, and return the Option if it is.
  ///
  /// ## Examples
  ///
  ///    > option(from("Hello"), string)
  ///    Ok(Some("Hello"))
  ///
  ///    > option(from("Hello"), string)
  ///    Ok(Some("Hello"))
  ///
  ///    > option(from(atom.from_string("null")), string)
  ///    Ok(None)
  ///
  ///    > option(from(atom.from_string("nil")), string)
  ///    Ok(None)
  ///
  ///    > option(from(atom.from_string("undefined")), string)
  ///    Ok(None)
  ///
  ///    > option(from(123), string)
  ///    Error("Expected a bit_string, found an int")
  ///
  pub external fn optional(
    from: Dynamic,
    of: Decoder(inner),
  ) -> Result(Option(inner), DecodeError) =
    "gleam_stdlib" "decode_optional"

  /// Checks to see if a Dynamic value is a map with a specific field, and return
  /// the value of the field if it is.
  ///
  /// This will not succeed on a record.
  ///
  /// ## Examples
  ///
  ///    > import gleam/map
  ///    > field(from(map.new("Hello", "World")), "Hello")
  ///    Ok(Dynamic)
  ///
  ///    > field(from(123), "Hello")
  ///    Error("Expected a map with key `\"Hello\"`, found an Int")
  ///
  pub external fn field(from: Dynamic, named: a) -> Result(Dynamic, DecodeError) =
    "gleam_stdlib" "decode_field"

  /// Checks to see if the Dynamic value is a tuple large enough to have a certain
  /// index, and return the value of that index if it is.
  ///
  /// ## Examples
  ///
  ///    > element(from(#(1, 2)), 0)
  ///    Ok(from(1))
  ///
  ///    > element(from(#(1, 2)), 2)
  ///    Error("Expected a tuple of at least 3 size, found a tuple of 2 size")
  ///
  ///    > element(from(""), 2)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn element(
    from: Dynamic,
    position: Int,
  ) -> Result(Dynamic, DecodeError) =
    "gleam_stdlib" "decode_element"

  /// Checks to see if the Dynamic value is a 2 element tuple.
  ///
  /// If you do not wish to decode all the elements in the tuple use the
  /// `typed_tuple2` function instead.
  ///
  /// ## Examples
  ///
  ///    > tuple2(from(#(1, 2)))
  ///    Ok(#(from(1), from(2)))
  ///
  ///    > tuple2(from(#(1, 2, 3)))
  ///    Error("Expected a 2 element tuple")
  ///
  ///    > tuple2(from(""))
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn tuple2(
    from: Dynamic,
  ) -> Result(#(Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_tuple2"

  /// Checks to see if the Dynamic value is a 2 element tuple containing two
  /// specifically typed elements.
  ///
  /// If you wish to decode all the elements in the list use the `typed_tuple2`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > typed_tuple2(from(#(1, 2)), int, int)
  ///    Ok(#(1, 2))
  ///
  ///    > typed_tuple2(from(#(1, 2.0)), int, float)
  ///    Ok(#(1, 2.0))
  ///
  ///    > typed_tuple2(from(#(1, 2, 3)), int, float)
  ///    Error("Expected a 2 element tuple, found a 3 element tuple")
  ///
  ///    > typed_tuple2(from(""), int, float)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub fn typed_tuple2(
    from tup: Dynamic,
    first decode_first: Decoder(a),
    second decode_second: Decoder(b),
  ) -> Result(#(a, b), DecodeError) {
    try #(first, second) = tuple2(tup)
    try a = decode_first(first)
    try b = decode_second(second)
    Ok(#(a, b))
  }

  /// Checks to see if the Dynamic value is a 3 element tuple.
  ///
  /// If you do not wish to decode all the elements in the tuple use the
  /// `typed_tuple3` function instead.
  ///
  /// ## Examples
  ///
  ///    > tuple3(from(#(1, 2, 3)))
  ///    Ok(#(from(1), from(2), from(3)))
  ///
  ///    > tuple3(from(#(1, 2)))
  ///    Error("Expected a 3 element tuple")
  ///
  ///    > tuple3(from(""))
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn tuple3(
    from: Dynamic,
  ) -> Result(#(Dynamic, Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_tuple3"

  /// Checks to see if the Dynamic value is a 3 element tuple containing two
  /// specifically typed elements.
  ///
  /// If you wish to decode all the elements in the list use the `typed_tuple3`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > typed_tuple3(from(#(1, 2, 3)), int, int, int)
  ///    Ok(#(1, 2, 3))
  ///
  ///    > typed_tuple3(from(#(1, 2.0, "3")), int, float, string)
  ///    Ok(#(1, 2.0, "3"))
  ///
  ///    > typed_tuple3(from(#(1, 2)), int, float, string)
  ///    Error("Expected a 3 element tuple, found a 2 element tuple")
  ///
  ///    > typed_tuple3(from(""), int, float, string)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub fn typed_tuple3(
    from tup: Dynamic,
    first decode_first: Decoder(a),
    second decode_second: Decoder(b),
    third decode_third: Decoder(c),
  ) -> Result(#(a, b, c), DecodeError) {
    try #(first, second, third) = tuple3(tup)
    try a = decode_first(first)
    try b = decode_second(second)
    try c = decode_third(third)
    Ok(#(a, b, c))
  }

  /// Checks to see if the Dynamic value is a 4 element tuple.
  ///
  /// If you do not wish to decode all the elements in the tuple use the
  /// `typed_tuple4` function instead.
  ///
  /// ## Examples
  ///
  ///    > tuple4(from(#(1, 2, 3, 4)))
  ///    Ok(#(from(1), from(2), from(3), from(4)))
  ///
  ///    > tuple4(from(#(1, 2)))
  ///    Error("Expected a 4 element tuple")
  ///
  ///    > tuple4(from(""))
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn tuple4(
    from: Dynamic,
  ) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_tuple4"

  /// Checks to see if the Dynamic value is a 4 element tuple containing two
  /// specifically typed elements.
  ///
  /// If you wish to decode all the elements in the list use the `typed_tuple4`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > typed_tuple4(from(#(1, 2, 3, 4)), int, int, int, int)
  ///    Ok(#(1, 2, 3, 4))
  ///
  ///    > typed_tuple4(from(#(1, 2.0, "3", 4)), int, float, string, int)
  ///    Ok(#(1, 2.0, "3", 4))
  ///
  ///    > typed_tuple4(from(#(1, 2)), int, float, string, int)
  ///    Error("Expected a 4 element tuple, found a 2 element tuple")
  ///
  ///    > typed_tuple4(from(""), int, float, string, int)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub fn typed_tuple4(
    from tup: Dynamic,
    first decode_first: Decoder(a),
    second decode_second: Decoder(b),
    third decode_third: Decoder(c),
    fourth decode_fourth: Decoder(d),
  ) -> Result(#(a, b, c, d), DecodeError) {
    try #(first, second, third, fourth) = tuple4(tup)
    try a = decode_first(first)
    try b = decode_second(second)
    try c = decode_third(third)
    try d = decode_fourth(fourth)
    Ok(#(a, b, c, d))
  }

  /// Checks to see if the Dynamic value is a 5 element tuple.
  ///
  /// If you do not wish to decode all the elements in the tuple use the
  /// `typed_tuple5` function instead.
  ///
  /// ## Examples
  ///
  ///    > tuple5(from(#(1, 2, 3, 4, 5)))
  ///    Ok(#(from(1), from(2), from(3), from(4), from(5)))
  ///
  ///    > tuple5(from(#(1, 2)))
  ///    Error("Expected a 5 element tuple")
  ///
  ///    > tuple5(from(""))
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn tuple5(
    from: Dynamic,
  ) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_tuple5"

  /// Checks to see if the Dynamic value is a 5 element tuple containing two
  /// specifically typed elements.
  ///
  /// If you wish to decode all the elements in the list use the `typed_tuple5`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > typed_tuple5(from(#(1, 2, 3, 4, 5)), int, int, int, int, int)
  ///    Ok(#(1, 2, 3, 4, 5))
  ///
  ///    > typed_tuple5(from(#(1, 2.0, "3", 4, 5)), int, float, string, int, int)
  ///    Ok(#(1, 2.0, "3", 4, 5))
  ///
  ///    > typed_tuple5(from(#(1, 2)), int, float, string, int, int)
  ///    Error("Expected a 5 element tuple, found a 2 element tuple")
  ///
  ///    > typed_tuple5(from(""), int, float, string, int, int)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub fn typed_tuple5(
    from tup: Dynamic,
    first decode_first: Decoder(a),
    second decode_second: Decoder(b),
    third decode_third: Decoder(c),
    fourth decode_fourth: Decoder(d),
    fifth decode_fifth: Decoder(e),
  ) -> Result(#(a, b, c, d, e), DecodeError) {
    try #(first, second, third, fourth, fifth) = tuple5(tup)
    try a = decode_first(first)
    try b = decode_second(second)
    try c = decode_third(third)
    try d = decode_fourth(fourth)
    try e = decode_fifth(fifth)
    Ok(#(a, b, c, d, e))
  }

  /// Checks to see if the Dynamic value is a 6 element tuple.
  ///
  /// If you do not wish to decode all the elements in the tuple use the
  /// `typed_tuple6` function instead.
  ///
  /// ## Examples
  ///
  ///    > tuple6(from(#(1, 2, 3, 4, 5, 6)))
  ///    Ok(#(from(1), from(2), from(3), from(4), from(5), from(6)))
  ///
  ///    > tuple6(from(#(1, 2)))
  ///    Error("Expected a 6 element tuple")
  ///
  ///    > tuple6(from(""))
  ///    Error("Expected a tuple, found a binary")
  ///
  pub external fn tuple6(
    from: Dynamic,
  ) -> Result(
    #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
    DecodeError,
  ) =
    "gleam_stdlib" "decode_tuple6"

  /// Checks to see if the Dynamic value is a 6 element tuple containing two
  /// specifically typed elements.
  ///
  /// If you wish to decode all the elements in the list use the `typed_tuple6`
  /// instead.
  ///
  /// ## Examples
  ///
  ///    > typed_tuple6(from(#(1, 2, 3, 4, 5, 6)), int, int, int, int, int, int)
  ///    Ok(#(1, 2, 3, 4, 5, 6))
  ///
  ///    > typed_tuple6(from(#(1, 2.0, "3", 4, 5, 6)), int, float, string, int, int)
  ///    Ok(#(1, 2.0, "3", 4, 5, 6))
  ///
  ///    > typed_tuple6(from(#(1, 2)), int, float, string, int, int, int)
  ///    Error("Expected a 6 element tuple, found a 2 element tuple")
  ///
  ///    > typed_tuple6(from(""), int, float, string, int, int, int)
  ///    Error("Expected a tuple, found a binary")
  ///
  pub fn typed_tuple6(
    from tup: Dynamic,
    first decode_first: Decoder(a),
    second decode_second: Decoder(b),
    third decode_third: Decoder(c),
    fourth decode_fourth: Decoder(d),
    fifth decode_fifth: Decoder(e),
    sixth decode_sixth: Decoder(f),
  ) -> Result(#(a, b, c, d, e, f), DecodeError) {
    try #(first, second, third, fourth, fifth, sixth) = tuple6(tup)
    try a = decode_first(first)
    try b = decode_second(second)
    try c = decode_third(third)
    try d = decode_fourth(fourth)
    try e = decode_fifth(fifth)
    try f = decode_sixth(sixth)
    Ok(#(a, b, c, d, e, f))
  }

  /// Checks to see if the Dynamic value is map.
  ///
  /// ## Examples
  ///
  ///    > import gleam/map
  ///    > map(from(map.new()))
  ///    Ok(map.new())
  ///
  ///    > map(from(1))
  ///    Error("Expected a 2 element tuple, found an int")
  ///
  ///    > map(from(""))
  ///    Error("Expected a map, found a binary")
  ///
  pub external fn map(
    from: Dynamic,
  ) -> Result(Map(Dynamic, Dynamic), DecodeError) =
    "gleam_stdlib" "decode_map"

  /// Joins multiple decoders into one. When run they will each be tried in turn
  /// until one succeeds, or they all fail.
  ///
  /// ## Examples
  ///
  ///    > import gleam/result
  ///    > let bool_or_string = any(_, of: [
  ///    >   string,
  ///    >   fn(x) { result.map(bool(x), fn(_) { "a bool" }) }
  ///    > ])
  ///    > bool_or_string(from("ok"))
  ///    Ok("ok")
  ///
  ///    > bool_or_string(from(True))
  ///    Ok("a bool")
  ///
  ///    > bool_or_string(from(1))
  ///    Error("Unexpected value")
  ///
  pub fn any(
    from data: Dynamic,
    of decoders: List(Decoder(t)),
  ) -> Result(t, DecodeError) {
    decoders
    |> list.find_map(fn(decoder) { decoder(data) })
    |> result.map_error(fn(_) {
      DecodeError(expected: "any", found: "unexpected")
    })
  }
}

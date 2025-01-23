import gleam/bit_array as bit_array_mod
import gleam/dict.{type Dict}
import gleam/int as int_mod
import gleam/list as list_mod
import gleam/option.{type Option, Some}
import gleam/result as result_mod
import gleam/string_tree

/// `Dynamic` data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
///
pub type Dynamic

/// Return a string indicating the type of the dynamic value.
///
/// This function may be useful for constructing error messages or logs. If you
/// want to turn dynamic data into well typed data then you want the
/// `gleam/dynamic/decode` module.
///
/// ```gleam
/// classify(from("Hello"))
/// // -> "String"
/// ```
///
@external(erlang, "gleam_stdlib", "classify_dynamic")
@external(javascript, "../gleam_stdlib.mjs", "classify_dynamic")
pub fn classify(data: Dynamic) -> String

pub type DecodeError {
  DecodeError(expected: String, found: String, path: List(String))
}

pub type DecodeErrors =
  List(DecodeError)

pub type Decoder(t) =
  fn(Dynamic) -> Result(t, DecodeErrors)

/// Converts any Gleam data into `Dynamic` data.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn from(a: anything) -> Dynamic

@deprecated("Please use the gleam/dynamic/decode module")
pub fn dynamic(value: Dynamic) -> Result(Dynamic, List(DecodeError)) {
  Ok(value)
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn bit_array(from data: Dynamic) -> Result(BitArray, DecodeErrors) {
  decode_bit_array(data)
}

@external(erlang, "gleam_stdlib", "decode_bit_array")
@external(javascript, "../gleam_stdlib.mjs", "decode_bit_array")
fn decode_bit_array(a: Dynamic) -> Result(BitArray, DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn string(from data: Dynamic) -> Result(String, DecodeErrors) {
  decode_string(data)
}

@external(javascript, "../gleam_stdlib.mjs", "decode_string")
fn decode_string(from data: Dynamic) -> Result(String, DecodeErrors) {
  decode_bit_array(data)
  |> map_errors(put_expected(_, "String"))
  |> result_mod.try(fn(raw) {
    case bit_array_mod.to_string(raw) {
      Ok(string) -> Ok(string)
      Error(Nil) ->
        Error([DecodeError(expected: "String", found: "BitArray", path: [])])
    }
  })
}

fn map_errors(
  result: Result(a, DecodeErrors),
  f: fn(DecodeError) -> DecodeError,
) -> Result(a, DecodeErrors) {
  result_mod.map_error(result, list_mod.map(_, f))
}

fn put_expected(error: DecodeError, expected: String) -> DecodeError {
  DecodeError(..error, expected: expected)
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn int(from data: Dynamic) -> Result(Int, DecodeErrors) {
  decode_int(data)
}

@external(erlang, "gleam_stdlib", "decode_int")
@external(javascript, "../gleam_stdlib.mjs", "decode_int")
fn decode_int(a: Dynamic) -> Result(Int, DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn float(from data: Dynamic) -> Result(Float, DecodeErrors) {
  decode_float(data)
}

@external(erlang, "gleam_stdlib", "decode_float")
@external(javascript, "../gleam_stdlib.mjs", "decode_float")
fn decode_float(a: Dynamic) -> Result(Float, DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn bool(from data: Dynamic) -> Result(Bool, DecodeErrors) {
  decode_bool(data)
}

@external(erlang, "gleam_stdlib", "decode_bool")
@external(javascript, "../gleam_stdlib.mjs", "decode_bool")
fn decode_bool(a: Dynamic) -> Result(Bool, DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn shallow_list(from value: Dynamic) -> Result(List(Dynamic), DecodeErrors) {
  decode_list(value)
}

@external(erlang, "gleam_stdlib", "decode_list")
@external(javascript, "../gleam_stdlib.mjs", "decode_list")
fn decode_list(a: Dynamic) -> Result(List(Dynamic), DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn result(
  ok decode_ok: Decoder(a),
  error decode_error: Decoder(e),
) -> Decoder(Result(a, e)) {
  fn(value) {
    use inner_result <- result_mod.try(decode_result(value))

    case inner_result {
      Ok(raw) -> {
        use value <- result_mod.try(
          decode_ok(raw)
          |> map_errors(push_path(_, "ok")),
        )
        Ok(Ok(value))
      }
      Error(raw) -> {
        use value <- result_mod.try(
          decode_error(raw)
          |> map_errors(push_path(_, "error")),
        )
        Ok(Error(value))
      }
    }
  }
}

@external(erlang, "gleam_stdlib", "decode_result")
@external(javascript, "../gleam_stdlib.mjs", "decode_result")
fn decode_result(a: Dynamic) -> Result(Result(a, e), DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn list(
  of decoder_type: fn(Dynamic) -> Result(inner, DecodeErrors),
) -> Decoder(List(inner)) {
  fn(dynamic) {
    use list <- result_mod.try(decode_list(dynamic))
    list
    |> list_mod.try_map(decoder_type)
    |> map_errors(push_path(_, "*"))
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn optional(of decode: Decoder(inner)) -> Decoder(Option(inner)) {
  fn(value) { decode_optional(value, decode) }
}

@external(erlang, "gleam_stdlib", "decode_option")
@external(javascript, "../gleam_stdlib.mjs", "decode_option")
fn decode_optional(a: Dynamic, b: Decoder(a)) -> Result(Option(a), DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn field(named name: a, of inner_type: Decoder(t)) -> Decoder(t) {
  fn(value) {
    let missing_field_error =
      DecodeError(expected: "field", found: "nothing", path: [])

    use maybe_inner <- result_mod.try(decode_field(value, name))
    maybe_inner
    |> option.to_result([missing_field_error])
    |> result_mod.try(inner_type)
    |> map_errors(push_path(_, name))
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn optional_field(
  named name: a,
  of inner_type: Decoder(t),
) -> Decoder(Option(t)) {
  fn(value) {
    use maybe_inner <- result_mod.try(decode_field(value, name))
    case maybe_inner {
      option.None -> Ok(option.None)
      option.Some(dynamic_inner) ->
        inner_type(dynamic_inner)
        |> result_mod.map(Some)
        |> map_errors(push_path(_, name))
    }
  }
}

@external(erlang, "gleam_stdlib", "decode_field")
@external(javascript, "../gleam_stdlib.mjs", "decode_field")
fn decode_field(a: Dynamic, b: name) -> Result(Option(Dynamic), DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn element(at index: Int, of inner_type: Decoder(inner)) -> Decoder(inner) {
  fn(data: Dynamic) {
    use tuple <- result_mod.try(decode_tuple(data))
    let size = tuple_size(tuple)
    use data <- result_mod.try(case index >= 0 {
      True ->
        case index < size {
          True -> tuple_get(tuple, index)
          False -> at_least_decode_tuple_error(index + 1, data)
        }
      False ->
        case int_mod.absolute_value(index) <= size {
          True -> tuple_get(tuple, size + index)
          False ->
            at_least_decode_tuple_error(int_mod.absolute_value(index), data)
        }
    })
    inner_type(data)
    |> map_errors(push_path(_, index))
  }
}

fn at_least_decode_tuple_error(
  size: Int,
  data: Dynamic,
) -> Result(a, DecodeErrors) {
  let s = case size {
    1 -> ""
    _ -> "s"
  }
  let error =
    ["Tuple of at least ", int_mod.to_string(size), " element", s]
    |> string_tree.from_strings
    |> string_tree.to_string
    |> DecodeError(found: classify(data), path: [])
  Error([error])
}

// A tuple of unknown size
type UnknownTuple

@external(erlang, "gleam_stdlib", "decode_tuple")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple")
fn decode_tuple(a: Dynamic) -> Result(UnknownTuple, DecodeErrors)

@external(erlang, "gleam_stdlib", "decode_tuple2")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple2")
fn decode_tuple2(a: Dynamic) -> Result(#(Dynamic, Dynamic), DecodeErrors)

@external(erlang, "gleam_stdlib", "decode_tuple3")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple3")
fn decode_tuple3(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic), DecodeErrors)

@external(erlang, "gleam_stdlib", "decode_tuple4")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple4")
fn decode_tuple4(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic), DecodeErrors)

@external(erlang, "gleam_stdlib", "decode_tuple5")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple5")
fn decode_tuple5(
  a: Dynamic,
) -> Result(#(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic), DecodeErrors)

@external(erlang, "gleam_stdlib", "decode_tuple6")
@external(javascript, "../gleam_stdlib.mjs", "decode_tuple6")
fn decode_tuple6(
  a: Dynamic,
) -> Result(
  #(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic),
  DecodeErrors,
)

@external(erlang, "gleam_stdlib", "tuple_get")
@external(javascript, "../gleam_stdlib.mjs", "tuple_get")
fn tuple_get(a: UnknownTuple, b: Int) -> Result(Dynamic, DecodeErrors)

@external(erlang, "gleam_stdlib", "size_of_tuple")
@external(javascript, "../gleam_stdlib.mjs", "length")
fn tuple_size(a: UnknownTuple) -> Int

fn tuple_errors(
  result: Result(a, List(DecodeError)),
  name: String,
) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> list_mod.map(errors, push_path(_, name))
  }
}

fn push_path(error: DecodeError, name: t) -> DecodeError {
  let name = from(name)
  let decoder =
    do_any([
      decode_string,
      fn(x) { result_mod.map(decode_int(x), int_mod.to_string) },
    ])
  let name = case decoder(name) {
    Ok(name) -> name
    Error(_) ->
      ["<", classify(name), ">"]
      |> string_tree.from_strings
      |> string_tree.to_string
  }
  DecodeError(..error, path: [name, ..error.path])
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn tuple2(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
) -> Decoder(#(a, b)) {
  fn(value) {
    use #(a, b) <- result_mod.try(decode_tuple2(value))
    case decode1(a), decode2(b) {
      Ok(a), Ok(b) -> Ok(#(a, b))
      a, b ->
        tuple_errors(a, "0")
        |> list_mod.append(tuple_errors(b, "1"))
        |> Error
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn tuple3(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
) -> Decoder(#(a, b, c)) {
  fn(value) {
    use #(a, b, c) <- result_mod.try(decode_tuple3(value))
    case decode1(a), decode2(b), decode3(c) {
      Ok(a), Ok(b), Ok(c) -> Ok(#(a, b, c))
      a, b, c ->
        tuple_errors(a, "0")
        |> list_mod.append(tuple_errors(b, "1"))
        |> list_mod.append(tuple_errors(c, "2"))
        |> Error
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn tuple4(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
) -> Decoder(#(a, b, c, d)) {
  fn(value) {
    use #(a, b, c, d) <- result_mod.try(decode_tuple4(value))
    case decode1(a), decode2(b), decode3(c), decode4(d) {
      Ok(a), Ok(b), Ok(c), Ok(d) -> Ok(#(a, b, c, d))
      a, b, c, d ->
        tuple_errors(a, "0")
        |> list_mod.append(tuple_errors(b, "1"))
        |> list_mod.append(tuple_errors(c, "2"))
        |> list_mod.append(tuple_errors(d, "3"))
        |> Error
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn tuple5(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
) -> Decoder(#(a, b, c, d, e)) {
  fn(value) {
    use #(a, b, c, d, e) <- result_mod.try(decode_tuple5(value))
    case decode1(a), decode2(b), decode3(c), decode4(d), decode5(e) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e) -> Ok(#(a, b, c, d, e))
      a, b, c, d, e ->
        tuple_errors(a, "0")
        |> list_mod.append(tuple_errors(b, "1"))
        |> list_mod.append(tuple_errors(c, "2"))
        |> list_mod.append(tuple_errors(d, "3"))
        |> list_mod.append(tuple_errors(e, "4"))
        |> Error
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn tuple6(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
  sixth decode6: Decoder(f),
) -> Decoder(#(a, b, c, d, e, f)) {
  fn(value) {
    use #(a, b, c, d, e, f) <- result_mod.try(decode_tuple6(value))
    case
      decode1(a),
      decode2(b),
      decode3(c),
      decode4(d),
      decode5(e),
      decode6(f)
    {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f) -> Ok(#(a, b, c, d, e, f))
      a, b, c, d, e, f ->
        tuple_errors(a, "0")
        |> list_mod.append(tuple_errors(b, "1"))
        |> list_mod.append(tuple_errors(c, "2"))
        |> list_mod.append(tuple_errors(d, "3"))
        |> list_mod.append(tuple_errors(e, "4"))
        |> list_mod.append(tuple_errors(f, "5"))
        |> Error
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn dict(
  of key_type: Decoder(k),
  to value_type: Decoder(v),
) -> Decoder(Dict(k, v)) {
  fn(value) {
    use dict <- result_mod.try(decode_dict(value))
    use pairs <- result_mod.try(
      dict
      |> dict.to_list
      |> list_mod.try_map(fn(pair) {
        let #(k, v) = pair
        use k <- result_mod.try(
          key_type(k)
          |> map_errors(push_path(_, "keys")),
        )
        use v <- result_mod.try(
          value_type(v)
          |> map_errors(push_path(_, "values")),
        )
        Ok(#(k, v))
      }),
    )
    Ok(dict.from_list(pairs))
  }
}

@external(erlang, "gleam_stdlib", "decode_map")
@external(javascript, "../gleam_stdlib.mjs", "decode_map")
fn decode_dict(a: Dynamic) -> Result(Dict(Dynamic, Dynamic), DecodeErrors)

@deprecated("Please use the gleam/dynamic/decode module")
pub fn any(of decoders: List(Decoder(a))) -> Decoder(a) {
  do_any(decoders)
}

fn do_any(of decoders: List(Decoder(a))) -> Decoder(a) {
  fn(data) {
    case decoders {
      [] ->
        Error([
          DecodeError(found: classify(data), expected: "another type", path: []),
        ])

      [decoder, ..decoders] ->
        case decoder(data) {
          Ok(decoded) -> Ok(decoded)
          Error(_) -> do_any(decoders)(data)
        }
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode1(constructor: fn(t1) -> t, t1: Decoder(t1)) -> Decoder(t) {
  fn(value) {
    case t1(value) {
      Ok(a) -> Ok(constructor(a))
      a -> Error(all_errors(a))
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode2(
  constructor: fn(t1, t2) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
) -> Decoder(t) {
  fn(value) {
    case t1(value), t2(value) {
      Ok(a), Ok(b) -> Ok(constructor(a, b))
      a, b -> Error(list_mod.flatten([all_errors(a), all_errors(b)]))
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode3(
  constructor: fn(t1, t2, t3) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
) -> Decoder(t) {
  fn(value) {
    case t1(value), t2(value), t3(value) {
      Ok(a), Ok(b), Ok(c) -> Ok(constructor(a, b, c))
      a, b, c ->
        Error(list_mod.flatten([all_errors(a), all_errors(b), all_errors(c)]))
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode4(
  constructor: fn(t1, t2, t3, t4) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x) {
      Ok(a), Ok(b), Ok(c), Ok(d) -> Ok(constructor(a, b, c, d))
      a, b, c, d ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
          ]),
        )
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode5(
  constructor: fn(t1, t2, t3, t4, t5) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e) -> Ok(constructor(a, b, c, d, e))
      a, b, c, d, e ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
          ]),
        )
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode6(
  constructor: fn(t1, t2, t3, t4, t5, t6) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f) ->
        Ok(constructor(a, b, c, d, e, f))
      a, b, c, d, e, f ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
          ]),
        )
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode7(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g) ->
        Ok(constructor(a, b, c, d, e, f, g))
      a, b, c, d, e, f, g ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
          ]),
        )
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode8(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7, t8) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
  t8: Decoder(t8),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x), t8(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g), Ok(h) ->
        Ok(constructor(a, b, c, d, e, f, g, h))
      a, b, c, d, e, f, g, h ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
            all_errors(h),
          ]),
        )
    }
  }
}

@deprecated("Please use the gleam/dynamic/decode module")
pub fn decode9(
  constructor: fn(t1, t2, t3, t4, t5, t6, t7, t8, t9) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
  t3: Decoder(t3),
  t4: Decoder(t4),
  t5: Decoder(t5),
  t6: Decoder(t6),
  t7: Decoder(t7),
  t8: Decoder(t8),
  t9: Decoder(t9),
) -> Decoder(t) {
  fn(x: Dynamic) {
    case t1(x), t2(x), t3(x), t4(x), t5(x), t6(x), t7(x), t8(x), t9(x) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f), Ok(g), Ok(h), Ok(i) ->
        Ok(constructor(a, b, c, d, e, f, g, h, i))
      a, b, c, d, e, f, g, h, i ->
        Error(
          list_mod.flatten([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
            all_errors(h),
            all_errors(i),
          ]),
        )
    }
  }
}

fn all_errors(result: Result(a, List(DecodeError))) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> errors
  }
}

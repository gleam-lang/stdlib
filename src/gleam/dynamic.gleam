import gleam/bit_string
import gleam/list
import gleam/map
import gleam/int
import gleam/option
import gleam/result
import gleam/string_builder
import gleam/map.{Map}
import gleam/option.{Option}

/// `Dynamic` data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
pub external type Dynamic

/// Error returned when unexpected data is encountered
pub type DecodeError {
  DecodeError(expected: String, found: String, path: List(String))
}

pub type DecodeErrors =
  List(DecodeError)

pub type Decoder(t) =
  fn(Dynamic) -> Result(t, DecodeErrors)

/// Converts any Gleam data into `Dynamic` data.
///
pub fn from(a) -> Dynamic {
  do_from(a)
}

if erlang {
  external fn do_from(anything) -> Dynamic =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from(anything) -> Dynamic =
    "../gleam_stdlib.mjs" "identity"
}

/// Unsafely casts a Dynamic value into any other type.
///
/// This is an escape hatch for the type system that may be useful when wrapping
/// native Erlang APIs. It is to be used as a last measure only!
///
/// If you can avoid using this function, do!
///
pub fn unsafe_coerce(a: Dynamic) -> anything {
  do_unsafe_coerce(a)
}

if erlang {
  external fn do_unsafe_coerce(Dynamic) -> a =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_unsafe_coerce(Dynamic) -> a =
    "../gleam_stdlib.mjs" "identity"
}

/// Converts a `Dynamic` value into a `Dynamic` value.
///
/// This function doesn't seem very useful at first, but it can be convenient
/// when you need to give a decoder function but you don't actually care what
/// the to-decode value is.
///
pub fn dynamic(term: Dynamic) -> Dynamic {
  unsafe_coerce(term)
}

/// Checks to see whether a `Dynamic` value is a bit_string, and returns that bit string if
/// it is.
///
/// ## Examples
///
///    > bit_string(from("Hello")) == bit_string.from_string("Hello")
///    True
///
///    > bit_string(from(123))
///    Error(DecodeError(expected: "BitString", found: "Int", path: []))
///
pub fn bit_string(from data: Dynamic) -> Result(BitString, DecodeErrors) {
  decode_bit_string(data)
}

if erlang {
  external fn decode_bit_string(Dynamic) -> Result(BitString, DecodeErrors) =
    "gleam_stdlib" "decode_bit_string"
}

if javascript {
  external fn decode_bit_string(Dynamic) -> Result(BitString, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_bit_string"
}

/// Checks to see whether a `Dynamic` value is a string, and returns that string if
/// it is.
///
/// ## Examples
///
///    > string(from("Hello"))
///    Ok("Hello")
///
///    > string(from(123))
///    Error(DecodeError(expected: "String", found: "Int", path: []))
///
pub fn string(from data: Dynamic) -> Result(String, DecodeErrors) {
  decode_string(data)
}

fn map_errors(
  result: Result(t, DecodeErrors),
  f: fn(DecodeError) -> DecodeError,
) -> Result(t, DecodeErrors) {
  result.map_error(result, list.map(_, f))
}

if erlang {
  fn decode_string(data: Dynamic) -> Result(String, DecodeErrors) {
    bit_string(data)
    |> map_errors(put_expected(_, "String"))
    |> result.then(fn(raw) {
      case bit_string.to_string(raw) {
        Ok(string) -> Ok(string)
        Error(Nil) ->
          Error([DecodeError(expected: "String", found: "BitString", path: [])])
      }
    })
  }
}

if javascript {
  external fn decode_string(Dynamic) -> Result(String, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_string"
}

/// Return a string indicating the type of the dynamic value.
///
/// ```
/// > classify(from("Hello"))
/// "String"
/// ```
///
pub fn classify(data: Dynamic) -> String {
  do_classify(data)
}

if erlang {
  external fn do_classify(Dynamic) -> String =
    "gleam_stdlib" "classify_dynamic"
}

if javascript {
  external fn do_classify(Dynamic) -> String =
    "../gleam_stdlib.mjs" "classify_dynamic"
}

/// Checks to see whether a `Dynamic` value is an int, and returns that int if it
/// is.
///
/// ## Examples
///
///    > int(from(123))
///    Ok(123)
///
///    > int(from("Hello"))
///    Error(DecodeError(expected: "Int", found: "String", path: []))
///
pub fn int(from data: Dynamic) -> Result(Int, DecodeErrors) {
  decode_int(data)
}

if erlang {
  external fn decode_int(Dynamic) -> Result(Int, DecodeErrors) =
    "gleam_stdlib" "decode_int"
}

if javascript {
  external fn decode_int(Dynamic) -> Result(Int, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_int"
}

/// Checks to see whether a `Dynamic` value is a float, and returns that float if
/// it is.
///
/// ## Examples
///
///    > float(from(2.0))
///    Ok(2.0)
///
///    > float(from(123))
///    Error(DecodeError(expected: "Float", found: "Int", path: []))
///
pub fn float(from data: Dynamic) -> Result(Float, DecodeErrors) {
  decode_float(data)
}

if erlang {
  external fn decode_float(Dynamic) -> Result(Float, DecodeErrors) =
    "gleam_stdlib" "decode_float"
}

if javascript {
  external fn decode_float(Dynamic) -> Result(Float, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_float"
}

/// Checks to see whether a `Dynamic` value is a bool, and returns that bool if
/// it is.
///
/// ## Examples
///
///    > bool(from(True))
///    Ok(True)
///
///    > bool(from(123))
///    Error(DecodeError(expected: "bool", found: "Int", path: []))
///
pub fn bool(from data: Dynamic) -> Result(Bool, DecodeErrors) {
  decode_bool(data)
}

if erlang {
  external fn decode_bool(Dynamic) -> Result(Bool, DecodeErrors) =
    "gleam_stdlib" "decode_bool"
}

if javascript {
  external fn decode_bool(Dynamic) -> Result(Bool, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_bool"
}

/// Checks to see whether a `Dynamic` value is a list, and returns that list if it
/// is. The types of the elements are not checked.
///
/// If you wish to decode all the elements in the list use the `list`
/// instead.
///
/// ## Examples
///
///    > shallow_list(from(["a", "b", "c"]))
///    Ok([from("a"), from("b"), from("c")])
///
///    > shallow_list(1)
///    Error(DecodeError(expected: "Int", found: "Int", path: []))
///
pub fn shallow_list(from value: Dynamic) -> Result(List(Dynamic), DecodeErrors) {
  decode_list(value)
}

if erlang {
  external fn decode_list(Dynamic) -> Result(List(Dynamic), DecodeErrors) =
    "gleam_stdlib" "decode_list"
}

if javascript {
  external fn decode_list(Dynamic) -> Result(List(Dynamic), DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_list"
}

if erlang {
  external fn decode_result(Dynamic) -> Result(Result(a, e), DecodeErrors) =
    "gleam_stdlib" "decode_result"
}

if javascript {
  external fn decode_result(Dynamic) -> Result(Result(a, e), DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_result"
}

/// Checks to see whether a `Dynamic` value is a result of a particular type, and
/// returns that result if it is.
///
/// The `ok` and `error` arguments are decoders for decoding the `Ok` and
/// `Error` values of the result.
///
/// ## Examples
///
///    > result(of: from(Ok(1)), ok: int, error: string)
///    Ok(Ok(1))
///
///    > result(of: from(Error("boom")), ok: int, error: string)
///    Ok(Error("boom"))
///
///    > result(of: from(123), ok: int, error: string)
///    Error(DecodeError(expected: "2 element tuple", found: "Int", path: []))
///
pub fn result(
  of dynamic: Dynamic,
  ok decode_ok: Decoder(a),
  error decode_error: Decoder(e),
) -> Result(Result(a, e), DecodeErrors) {
  try inner_result = decode_result(dynamic)

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

/// Checks to see whether a `Dynamic` value is a list of a particular type, and
/// returns that list if it is.
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
///    > list(from(["a", "b", "c"]), of: string)
///    Ok(["a", "b", "c"])
///
///    > list(from([1, 2, 3]), of: string)
///    Error(DecodeError(expected: "String", found: "Int", path: []))
///
///    > list(from("ok"), of: string)
///    Error(DecodeError(expected: "List", found: "String", path: []))
///
pub fn list(
  from dynamic: Dynamic,
  of decoder_type: fn(Dynamic) -> Result(inner, DecodeErrors),
) -> Result(List(inner), DecodeErrors) {
  try list = shallow_list(dynamic)
  list
  |> list.try_map(decoder_type)
  |> map_errors(push_path(_, "*"))
}

/// Checks to see if a `Dynamic` value is a nullable version of a particular
/// type, and returns a corresponding `Option` if it is.
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
///    Error(DecodeError(expected: "BitString", found: "Int", path: []))
///
pub fn optional(
  from value: Dynamic,
  of decode: Decoder(inner),
) -> Result(Option(inner), DecodeErrors) {
  decode_optional(value, decode)
}

if erlang {
  external fn decode_optional(
    Dynamic,
    Decoder(a),
  ) -> Result(Option(a), DecodeErrors) =
    "gleam_stdlib" "decode_option"
}

if javascript {
  external fn decode_optional(
    Dynamic,
    Decoder(a),
  ) -> Result(Option(a), DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_option"
}

/// Checks to see if a `Dynamic` value is a map with a specific field, and returns
/// the value of that field if it is.
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
///    Error(DecodeError(expected: "Map", found: "Int", path: []))
///
pub fn field(
  from value: Dynamic,
  named name: a,
  of inner_type: Decoder(t),
) -> Result(t, DecodeErrors) {
  try value = decode_field(value, name)
  inner_type(value)
  |> map_errors(push_path(_, name))
}

if erlang {
  external fn decode_field(Dynamic, name) -> Result(Dynamic, DecodeErrors) =
    "gleam_stdlib" "decode_field"
}

if javascript {
  external fn decode_field(Dynamic, name) -> Result(Dynamic, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_field"
}

/// Checks to see if a `Dynamic` value is a tuple large enough to have a certain
/// index, and returns the value of that index if it is.
///
/// ## Examples
///
///    > element(from(#(1, 2)), 0)
///    Ok(from(1))
///
///    > element(from(#(1, 2)), 2)
///    Error(DecodeError(expected: "3 element tuple", found: "2 element tuple", path: []))
///
///    > element(from(""), 2)
///    Error(DecodeError(expected: "Tuple", found: "String", path: []))
///
pub fn element(
  from data: Dynamic,
  at index: Int,
  of inner_type: Decoder(t),
) -> Result(t, DecodeErrors) {
  try tuple = decode_tuple(data)
  let size = tuple_size(tuple)
  try data = case index >= 0 {
    True ->
      case index < size {
        True -> tuple_get(tuple, index)
        False -> at_least_decode_tuple_error(index + 1, data)
      }
    False ->
      case int.absolute_value(index) <= size {
        True -> tuple_get(tuple, size + index)
        False -> at_least_decode_tuple_error(int.absolute_value(index), data)
      }
  }
  inner_type(data)
  |> map_errors(push_path(_, index))
}

fn exact_decode_tuple_error(size: Int, data: Dynamic) -> Result(a, DecodeErrors) {
  let s = case size {
    0 -> ""
    _ -> "s"
  }
  let error =
    ["Tuple of ", int.to_string(size), " element", s]
    |> string_builder.from_strings
    |> string_builder.to_string
    |> DecodeError(found: classify(data), path: [])
  Error([error])
}

fn at_least_decode_tuple_error(
  size: Int,
  data: Dynamic,
) -> Result(a, DecodeErrors) {
  let s = case size {
    0 -> ""
    _ -> "s"
  }
  let error =
    ["Tuple of at least ", int.to_string(size), " element", s]
    |> string_builder.from_strings
    |> string_builder.to_string
    |> DecodeError(found: classify(data), path: [])
  Error([error])
}

// A tuple of unknown size
external type UnknownTuple

if erlang {
  external fn decode_tuple(Dynamic) -> Result(UnknownTuple, DecodeErrors) =
    "gleam_stdlib" "decode_tuple"

  external fn tuple_get(UnknownTuple, Int) -> Result(Dynamic, DecodeErrors) =
    "gleam_stdlib" "tuple_get"

  external fn tuple_size(UnknownTuple) -> Int =
    "gleam_stdlib" "size_of_tuple"
}

if javascript {
  external fn decode_tuple(Dynamic) -> Result(UnknownTuple, DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_tuple"

  external fn tuple_get(UnknownTuple, Int) -> Result(Dynamic, DecodeErrors) =
    "../gleam_stdlib.mjs" "tuple_get"

  external fn tuple_size(UnknownTuple) -> Int =
    "../gleam_stdlib.mjs" "length"
}

/// Checks to see if a `Dynamic` value is a 2 element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple2(from(#(1, 2)), int, int)
///    Ok(#(1, 2))
///
///    > tuple2(from(#(1, 2.0)), int, float)
///    Ok(#(1, 2.0))
///
///    > tuple2(from(#(1, 2, 3)), int, float)
///    Error(DecodeError(expected: "2 element tuple", found: "3 element tuple", path: []))
///
///    > tuple2(from(""), int, float)
///    Error(DecodeError(expected: "2 element tuple", found: "String", path: []))
///
pub fn tuple2(
  from value: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
) -> Result(#(a, b), DecodeErrors) {
  try _ = assert_is_tuple(value, 2)
  let #(first, second) = unsafe_coerce(value)
  try a =
    decode_first(first)
    |> map_errors(push_path(_, "0"))
  try b =
    decode_second(second)
    |> map_errors(push_path(_, "1"))
  Ok(#(a, b))
}

fn assert_is_tuple(
  value: Dynamic,
  desired_size: Int,
) -> Result(Nil, DecodeErrors) {
  let expected =
    string_builder.to_string(string_builder.from_strings([
      "Tuple of ",
      int.to_string(desired_size),
      " elements",
    ]))
  try tuple = map_errors(decode_tuple(value), put_expected(_, expected))
  case tuple_size(tuple) {
    size if size == desired_size -> Ok(Nil)
    _ -> exact_decode_tuple_error(desired_size, value)
  }
}

fn put_expected(error: DecodeError, expected: String) -> DecodeError {
  DecodeError(..error, expected: expected)
}

fn push_path(error: DecodeError, name: t) -> DecodeError {
  let name = from(name)
  let decoder = any(_, [string, fn(x) { result.map(int(x), int.to_string) }])
  let name = case decoder(name) {
    Ok(name) -> name
    Error(_) ->
      ["<", classify(name), ">"]
      |> string_builder.from_strings
      |> string_builder.to_string
  }
  DecodeError(..error, path: [name, ..error.path])
}

/// Checks to see if a `Dynamic` value is a 3-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple3(from(#(1, 2, 3)), int, int, int)
///    Ok(#(1, 2, 3))
///
///    > tuple3(from(#(1, 2.0, "3")), int, float, string)
///    Ok(#(1, 2.0, "3"))
///
///    > tuple3(from(#(1, 2)), int, float, string)
///    Error(DecodeError(expected: "3 element tuple", found: "2 element tuple", path: []))
///
///    > tuple3(from(""), int, float, string)
///    Error(DecodeError(expected: "3 element tuple", found: "String", path: []))
///
pub fn tuple3(
  from value: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
) -> Result(#(a, b, c), DecodeErrors) {
  try _ = assert_is_tuple(value, 3)
  let #(first, second, third) = unsafe_coerce(value)
  try a =
    decode_first(first)
    |> map_errors(push_path(_, "0"))
  try b =
    decode_second(second)
    |> map_errors(push_path(_, "1"))
  try c =
    decode_third(third)
    |> map_errors(push_path(_, "2"))
  Ok(#(a, b, c))
}

/// Checks to see if a `Dynamic` value is a 4 element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple4(from(#(1, 2, 3, 4)), int, int, int, int)
///    Ok(#(1, 2, 3, 4))
///
///    > tuple4(from(#(1, 2.0, "3", 4)), int, float, string, int)
///    Ok(#(1, 2.0, "3", 4))
///
///    > tuple4(from(#(1, 2)), int, float, string, int)
///    Error("Expected a 4 element tuple, found a 2 element tuple")
///    Error(DecodeError(expected: "4 element tuple", found: "2 element tuple", path: []))
///
///    > tuple4(from(""), int, float, string, int)
///    Error(DecodeError(expected: "4 element tuple", found: "String", path: []))
///
pub fn tuple4(
  from value: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
) -> Result(#(a, b, c, d), DecodeErrors) {
  try _ = assert_is_tuple(value, 4)
  let #(first, second, third, fourth) = unsafe_coerce(value)
  try a =
    decode_first(first)
    |> map_errors(push_path(_, "0"))
  try b =
    decode_second(second)
    |> map_errors(push_path(_, "1"))
  try c =
    decode_third(third)
    |> map_errors(push_path(_, "2"))
  try d =
    decode_fourth(fourth)
    |> map_errors(push_path(_, "3"))
  Ok(#(a, b, c, d))
}

/// Checks to see if a `Dynamic` value is a 5-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple5(from(#(1, 2, 3, 4, 5)), int, int, int, int, int)
///    Ok(#(1, 2, 3, 4, 5))
///
///    > tuple5(from(#(1, 2.0, "3", 4, 5)), int, float, string, int, int)
///    Ok(#(1, 2.0, "3", 4, 5))
///
///    > tuple5(from(#(1, 2)), int, float, string, int, int)
///    Error(DecodeError(expected: "5 element tuple", found: "2 element tuple", path: []))
///
///    > tuple5(from(""), int, float, string, int, int)
///    Error(DecodeError(expected: "5 element tuple", found: "String", path: []))
///
pub fn tuple5(
  from value: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
  fifth decode_fifth: Decoder(e),
) -> Result(#(a, b, c, d, e), DecodeErrors) {
  try _ = assert_is_tuple(value, 5)
  let #(first, second, third, fourth, fifth) = unsafe_coerce(value)
  try a =
    decode_first(first)
    |> map_errors(push_path(_, "0"))
  try b =
    decode_second(second)
    |> map_errors(push_path(_, "1"))
  try c =
    decode_third(third)
    |> map_errors(push_path(_, "2"))
  try d =
    decode_fourth(fourth)
    |> map_errors(push_path(_, "3"))
  try e =
    decode_fifth(fifth)
    |> map_errors(push_path(_, "4"))
  Ok(#(a, b, c, d, e))
}

/// Checks to see if a `Dynamic` value is a 6-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple6(from(#(1, 2, 3, 4, 5, 6)), int, int, int, int, int, int)
///    Ok(#(1, 2, 3, 4, 5, 6))
///
///    > tuple6(from(#(1, 2.0, "3", 4, 5, 6)), int, float, string, int, int)
///    Ok(#(1, 2.0, "3", 4, 5, 6))
///
///    > tuple6(from(#(1, 2)), int, float, string, int, int, int)
///    Error(DecodeError(expected: "6 element tuple", found: "2 element tuple", path: []))
///
///    > tuple6(from(""), int, float, string, int, int, int)
///    Error(DecodeError(expected: "6 element tuple", found: "String", path: []))
///
pub fn tuple6(
  from value: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
  third decode_third: Decoder(c),
  fourth decode_fourth: Decoder(d),
  fifth decode_fifth: Decoder(e),
  sixth decode_sixth: Decoder(f),
) -> Result(#(a, b, c, d, e, f), DecodeErrors) {
  try _ = assert_is_tuple(value, 6)
  let #(first, second, third, fourth, fifth, sixth) = unsafe_coerce(value)
  try a =
    decode_first(first)
    |> map_errors(push_path(_, "0"))
  try b =
    decode_second(second)
    |> map_errors(push_path(_, "1"))
  try c =
    decode_third(third)
    |> map_errors(push_path(_, "2"))
  try d =
    decode_fourth(fourth)
    |> map_errors(push_path(_, "3"))
  try e =
    decode_fifth(fifth)
    |> map_errors(push_path(_, "4"))
  try f =
    decode_sixth(sixth)
    |> map_errors(push_path(_, "5"))
  Ok(#(a, b, c, d, e, f))
}

/// Checks to see if a `Dynamic` value is a map.
///
/// ## Examples
///
///    > import gleam/map
///    > map(from(map.new()))
///    Ok(map.new())
///
///    > map(from(1))
///    Error(DecodeError(expected: "Map", found: "Int", path: []))
///
///    > map(from(""))
///    Error(DecodeError(expected: "Map", found: "String", path: []))
///
pub fn map(from value: Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeErrors) {
  decode_map(value)
}

if erlang {
  external fn decode_map(Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeErrors) =
    "gleam_stdlib" "decode_map"
}

if javascript {
  external fn decode_map(Dynamic) -> Result(Map(Dynamic, Dynamic), DecodeErrors) =
    "../gleam_stdlib.mjs" "decode_map"
}

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
///    Error(DecodeError(expected: "unknown", found: "unknown", path: []))
///
pub fn any(
  from data: Dynamic,
  of decoders: List(Decoder(t)),
) -> Result(t, DecodeErrors) {
  case decoders {
    [] ->
      Error([
        DecodeError(found: classify(data), expected: "another type", path: []),
      ])

    [decoder, ..decoders] ->
      case decoder(data) {
        Ok(decoded) -> Ok(decoded)
        Error(_) -> any(data, decoders)
      }
  }
}

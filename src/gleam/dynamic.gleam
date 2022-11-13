import gleam/int
import gleam/list
import gleam/map.{Map}
import gleam/option.{Option}
import gleam/result
import gleam/string_builder

if erlang {
  import gleam/bit_string
}

/// `Dynamic` data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
///
pub external type Dynamic

/// Error returned when unexpected data is encountered
///
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

/// Decodes a `Dynamic` value from a `Dynamic` value.
///
/// This function doesn't seem very useful at first, but it can be convenient
/// when you need to give a decoder function but you don't actually care what
/// the to-decode value is.
///
pub fn dynamic(value: Dynamic) -> Result(Dynamic, List(DecodeError)) {
  Ok(value)
}

/// Checks to see whether a `Dynamic` value is a bit_string, and returns that bit string if
/// it is.
///
/// ## Examples
///
/// ```gleam
/// > bit_string(from("Hello")) == bit_string.from_string("Hello")
/// True
/// ```
///
/// ```gleam
/// > bit_string(from(123))
/// Error([DecodeError(expected: "BitString", found: "Int", path: [])])
/// ```
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
/// ```gleam
/// > string(from("Hello"))
/// Ok("Hello")
/// ```
///
/// ```gleam
/// > string(from(123))
/// Error([DecodeError(expected: "String", found: "Int", path: [])])
/// ```
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
/// ```gleam
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
/// ```gleam
/// > int(from(123))
/// Ok(123)
/// ```
///
/// ```gleam
/// > int(from("Hello"))
/// Error([DecodeError(expected: "Int", found: "String", path: [])])
/// ```
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
/// ```gleam
/// > float(from(2.0))
/// Ok(2.0)
/// ```
///
/// ```gleam
/// > float(from(123))
/// Error([DecodeError(expected: "Float", found: "Int", path: [])])
/// ```
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
/// ```gleam
/// > bool(from(True))
/// Ok(True)
/// ```
///
/// ```gleam
/// > bool(from(123))
/// Error([DecodeError(expected: "bool", found: "Int", path: [])])
/// ```
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
/// If you wish to decode all the elements in the list use the `list` function
/// instead.
///
/// ## Examples
///
/// ```gleam
/// > shallow_list(from(["a", "b", "c"]))
/// Ok([from("a"), from("b"), from("c")])
/// ```
///
/// ```gleam
/// > shallow_list(1)
/// Error([DecodeError(expected: "Int", found: "Int", path: [])])
/// ```
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
/// ```gleam
/// > from(Ok(1))
/// > |> result(ok: int, error: string)
/// Ok(Ok(1))
/// ```
///
/// ```gleam
/// > from(Error("boom"))
/// > |> result(ok: int, error: string)
/// Ok(Error("boom"))
/// ```
///
/// ```gleam
/// > from(123)
/// > |> result(ok: int, error: string)
/// Error([DecodeError(expected: "2 element tuple", found: "Int", path: [])])
/// ```
///
pub fn result(
  ok decode_ok: Decoder(a),
  error decode_error: Decoder(e),
) -> Decoder(Result(a, e)) {
  fn(value) {
    try inner_result = decode_result(value)

    case inner_result {
      Ok(raw) -> {
        try value =
          decode_ok(raw)
          |> map_errors(push_path(_, "ok"))
        Ok(Ok(value))
      }
      Error(raw) -> {
        try value =
          decode_error(raw)
          |> map_errors(push_path(_, "error"))
        Ok(Error(value))
      }
    }
  }
}

/// Checks to see whether a `Dynamic` value is a list of a particular type, and
/// returns that list if it is.
///
/// The second argument is a decoder function used to decode the elements of
/// the list. The list is only decoded if all elements in the list can be
/// successfully decoded using this function.
///
/// If you do not wish to decode all the elements in the list use the `shallow_list`
/// function instead.
///
/// ## Examples
///
/// ```gleam
/// > from(["a", "b", "c"])
/// > |> list(of: string)
/// Ok(["a", "b", "c"])
/// ```
///
/// ```gleam
/// > from([1, 2, 3])
/// > |> list(of: string)
/// Error([DecodeError(expected: "String", found: "Int", path: [])])
/// ```
///
/// ```gleam
/// > from("ok")
/// > |> list(of: string)
/// Error([DecodeError(expected: "List", found: "String", path: [])])
/// ```
///
pub fn list(
  of decoder_type: fn(Dynamic) -> Result(inner, DecodeErrors),
) -> Decoder(List(inner)) {
  fn(dynamic) {
    try list = shallow_list(dynamic)
    list
    |> list.try_map(decoder_type)
    |> map_errors(push_path(_, "*"))
  }
}

/// Checks to see if a `Dynamic` value is a nullable version of a particular
/// type, and returns a corresponding `Option` if it is.
///
/// ## Examples
///
/// ```gleam
/// > from("Hello")
/// > |> optional(string)
/// Ok(Some("Hello"))
/// ```
///
/// ```gleam
/// > from("Hello")
/// > |> optional(string)
/// Ok(Some("Hello"))
/// ```
///
/// ```gleam
/// > from(atom.from_string("null"))
/// > |> optional(string)
/// Ok(None)
/// ```
///
/// ```gleam
/// > from(atom.from_string("nil"))
/// > |> optional(string)
/// Ok(None)
/// ```
///
/// ```gleam
/// > from(atom.from_string("undefined"))
/// > |> optional(string)
/// Ok(None)
/// ```
///
/// ```gleam
/// > from(123)
/// > |> optional(string)
/// Error([DecodeError(expected: "BitString", found: "Int", path: [])])
/// ```
///
pub fn optional(of decode: Decoder(inner)) -> Decoder(Option(inner)) {
  fn(value) { decode_optional(value, decode) }
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
/// ```gleam
/// > import gleam/map
/// > from(map.new("Hello", "World"))
/// > |> field(named: "Hello", of: string)
/// Ok("World")
/// ```
///
/// ```gleam
/// > from(123)
/// > |> field("Hello", string)
/// Error([DecodeError(expected: "Map", found: "Int", path: [])])
/// ```
///
pub fn field(named name: a, of inner_type: Decoder(t)) -> Decoder(t) {
  fn(value) {
    value
    |> decode_field(name)
    |> result.then(inner_type)
    |> map_errors(push_path(_, name))
  }
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
/// ```gleam
/// > from(#(1, 2))
/// > |> element(0, int)
/// Ok(from(1))
/// ```
///
/// ```gleam
/// > from(#(1, 2))
/// > |> element(2, int)
/// Error([
///   DecodeError(
///     expected: "Tuple of at least 3 elements",
///     found: "Tuple of 2 elements",
///     path: [],
///   ),
/// ])
/// ```
///
pub fn element(at index: Int, of inner_type: Decoder(t)) -> Decoder(t) {
  fn(data: Dynamic) {
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

fn tuple_errors(
  result: Result(a, List(DecodeError)),
  name: String,
) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> list.map(errors, push_path(_, name))
  }
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
  let decoder = any([string, fn(x) { result.map(int(x), int.to_string) }])
  let name = case decoder(name) {
    Ok(name) -> name
    Error(_) ->
      ["<", classify(name), ">"]
      |> string_builder.from_strings
      |> string_builder.to_string
  }
  DecodeError(..error, path: [name, ..error.path])
}

/// Checks to see if a `Dynamic` value is a 2 element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2))
/// > |> tuple2(int, int)
/// Ok(#(1, 2))
/// ```
///
/// ```gleam
/// > from(#(1, 2.0))
/// > |> tuple2(int, float)
/// Ok(#(1, 2.0))
/// ```
///
/// ```gleam
/// > from(#(1, 2, 3))
/// > |> tuple2(int, float)
/// Error([
///   DecodeError(expected: "2 element tuple", found: "3 element tuple", path: []),
/// ])
/// ```
///
/// ```gleam
/// > from("")
/// > |> tuple2(int, float)
/// Error([DecodeError(expected: "2 element tuple", found: "String", path: [])])
/// ```
///
pub fn tuple2(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
) -> Decoder(#(a, b)) {
  fn(value) {
    try _ = assert_is_tuple(value, 2)
    let #(a, b) = unsafe_coerce(value)
    case decode1(a), decode2(b) {
      Ok(a), Ok(b) -> Ok(#(a, b))
      a, b ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> Error
    }
  }
}

/// Checks to see if a `Dynamic` value is a 3-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2, 3))
/// > |> tuple3(int, int, int)
/// Ok(#(1, 2, 3))
/// ```
///
/// ```gleam
/// > from(#(1, 2.0, "3"))
/// > |> tuple3(int, float, string)
/// Ok(#(1, 2.0, "3"))
/// ```
///
/// ```gleam
/// > from(#(1, 2))
/// > |> tuple3(int, float, string)
/// Error([
///   DecodeError(expected: "3 element tuple", found: "2 element tuple", path: [])),
/// ])
/// ```
///
/// ```gleam
/// > from("")
/// > |> tuple3(int, float, string)
/// Error([
///   DecodeError(expected: "3 element tuple", found: "String", path: []),
/// ])
/// ```
///
pub fn tuple3(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
) -> Decoder(#(a, b, c)) {
  fn(value) {
    try _ = assert_is_tuple(value, 3)
    let #(a, b, c) = unsafe_coerce(value)
    case decode1(a), decode2(b), decode3(c) {
      Ok(a), Ok(b), Ok(c) -> Ok(#(a, b, c))
      a, b, c ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> Error
    }
  }
}

/// Checks to see if a `Dynamic` value is a 4 element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2, 3, 4))
/// > |> tuple4(int, int, int, int)
/// Ok(#(1, 2, 3, 4))
/// ```
///
/// ```gleam
/// > from(#(1, 2.0, "3", 4))
/// > |> tuple4(int, float, string, int)
/// Ok(#(1, 2.0, "3", 4))
///
/// > from(#(1, 2))
/// > |> tuple4(int, float, string, int)
/// Error([
///   DecodeError(expected: "4 element tuple", found: "2 element tuple", path: []),
/// ])
/// ```
///
/// ```gleam
/// > from("")
/// > |> tuple4(int, float, string, int)
/// Error([
///   DecodeError(expected: "4 element tuple", found: "String", path: []),
/// ])
/// ```
///
pub fn tuple4(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
) -> Decoder(#(a, b, c, d)) {
  fn(value) {
    try _ = assert_is_tuple(value, 4)
    let #(a, b, c, d) = unsafe_coerce(value)
    case decode1(a), decode2(b), decode3(c), decode4(d) {
      Ok(a), Ok(b), Ok(c), Ok(d) -> Ok(#(a, b, c, d))
      a, b, c, d ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> Error
    }
  }
}

/// Checks to see if a `Dynamic` value is a 5-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2, 3, 4, 5))
/// > |> tuple5(int, int, int, int, int)
/// Ok(#(1, 2, 3, 4, 5))
/// ```
///
/// ```gleam
/// > from(#(1, 2.0, "3", 4, 5))
/// > |> tuple5(int, float, string, int, int)
/// Ok(#(1, 2.0, "3", 4, 5))
/// ```
///
/// ```gleam
/// > from(#(1, 2))
/// > |> tuple5(int, float, string, int, int)
/// Error([
///   DecodeError(expected: "5 element tuple", found: "2 element tuple", path: [])),
/// ])
///
/// > from("")
/// > |> tuple5(int, float, string, int, int)
/// Error([DecodeError(expected: "5 element tuple", found: "String", path: [])])
/// ```
///
pub fn tuple5(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
) -> Decoder(#(a, b, c, d, e)) {
  fn(value) {
    try _ = assert_is_tuple(value, 5)
    let #(a, b, c, d, e) = unsafe_coerce(value)
    case decode1(a), decode2(b), decode3(c), decode4(d), decode5(e) {
      Ok(a), Ok(b), Ok(c), Ok(d), Ok(e) -> Ok(#(a, b, c, d, e))
      a, b, c, d, e ->
        tuple_errors(a, "0")
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> list.append(tuple_errors(e, "4"))
        |> Error
    }
  }
}

/// Checks to see if a `Dynamic` value is a 6-element tuple containing
/// specifically typed elements.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2, 3, 4, 5, 6))
/// > |> tuple6(int, int, int, int, int, int)
/// Ok(#(1, 2, 3, 4, 5, 6))
/// ```
///
/// ```gleam
/// > from(#(1, 2.0, "3", 4, 5, 6))
/// > |> tuple6(int, float, string, int, int)
/// Ok(#(1, 2.0, "3", 4, 5, 6))
/// ```
///
/// ```gleam
/// > from(#(1, 2))
/// > |> tuple6(int, float, string, int, int, int)
/// Error([
///   DecodeError(expected: "6 element tuple", found: "2 element tuple", path: []),
/// ])
/// ```
///
pub fn tuple6(
  first decode1: Decoder(a),
  second decode2: Decoder(b),
  third decode3: Decoder(c),
  fourth decode4: Decoder(d),
  fifth decode5: Decoder(e),
  sixth decode6: Decoder(f),
) -> Decoder(#(a, b, c, d, e, f)) {
  fn(value) {
    try _ = assert_is_tuple(value, 6)
    let #(a, b, c, d, e, f) = unsafe_coerce(value)
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
        |> list.append(tuple_errors(b, "1"))
        |> list.append(tuple_errors(c, "2"))
        |> list.append(tuple_errors(d, "3"))
        |> list.append(tuple_errors(e, "4"))
        |> list.append(tuple_errors(f, "5"))
        |> Error
    }
  }
}

/// Checks to see if a `Dynamic` value is a map.
///
/// ## Examples
///
/// ```gleam
/// > import gleam/map
/// > map.new() |> from |> map(string, int)
/// Ok(map.new())
/// ```
///
/// ```gleam
/// > from(1) |> map(string, int)
/// Error(DecodeError(expected: "Map", found: "Int", path: []))
/// ```
///
/// ```gleam
/// > from("") |> map(string, int)
/// Error(DecodeError(expected: "Map", found: "String", path: []))
/// ```
///
pub fn map(
  of key_type: Decoder(k),
  to value_type: Decoder(v),
) -> Decoder(Map(k, v)) {
  fn(value) {
    try map = decode_map(value)
    try pairs =
      map
      |> map.to_list
      |> list.try_map(fn(pair) {
        let #(k, v) = pair
        try k =
          key_type(k)
          |> map_errors(push_path(_, "keys"))
        try v =
          value_type(v)
          |> map_errors(push_path(_, "values"))
        Ok(#(k, v))
      })
    Ok(map.from_list(pairs))
  }
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
/// ```gleam
/// > import gleam/result
/// > let bool_or_string = any(of: [
/// >   string,
/// >   fn(x) { result.map(bool(x), fn(_) { "a bool" }) }
/// > ])
/// > bool_or_string(from("ok"))
/// Ok("ok")
/// ```
///
/// ```gleam
/// > bool_or_string(from(True))
/// Ok("a bool")
/// ```
///
/// ```gleam
/// > bool_or_string(from(1))
/// Error(DecodeError(expected: "unknown", found: "unknown", path: []))
/// ```
///
pub fn any(of decoders: List(Decoder(t))) -> Decoder(t) {
  fn(data) {
    case decoders {
      [] ->
        Error([
          DecodeError(found: classify(data), expected: "another type", path: []),
        ])

      [decoder, ..decoders] ->
        case decoder(data) {
          Ok(decoded) -> Ok(decoded)
          Error(_) -> any(decoders)(data)
        }
    }
  }
}

/// Decode 2 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.0, "3"))
/// > |> decode2(MyRecord, element(0, int), element(1, float))
/// Ok(MyRecord(1, 2.0))
/// ```
///
/// ```gleam
/// > from(#("", "", ""))
/// > |> decode2(MyRecord, element(0, int), element(1, float))
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
pub fn decode2(
  constructor: fn(t1, t2) -> t,
  t1: Decoder(t1),
  t2: Decoder(t2),
) -> Decoder(t) {
  fn(value) {
    case t1(value), t2(value) {
      Ok(a), Ok(b) -> Ok(constructor(a, b))
      a, b -> Error(list.flatten([all_errors(a), all_errors(b)]))
    }
  }
}

/// Decode 3 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.0, "3"))
/// > |> decode3(MyRecord, element(0, int), element(1, float), element(2, string))
/// Ok(MyRecord(1, 2.0, "3"))
/// ```
///
/// ```gleam
/// > from(#("", "", ""))
/// > |> decode3(MyRecord, element(0, int), element(1, float), element(2, string))
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([all_errors(a), all_errors(b), all_errors(c)]))
    }
  }
}

/// Decode 4 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4"))
/// > |> decode4(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", ""))
/// > |> decode4(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
        ]))
    }
  }
}

/// Decode 5 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4", "5"))
/// > |> decode5(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4", "5"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", "", ""))
/// > |> decode5(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
        ]))
    }
  }
}

/// Decode 6 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4", "5", "6"))
/// > |> decode6(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4", "5", "6"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", "", "", ""))
/// > |> decode6(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
        ]))
    }
  }
}

/// Decode 7 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4", "5", "6"))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4", "5", "6", "7"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", "", "", "", ""))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
        ]))
    }
  }
}

/// Decode 8 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4", "5", "6", "7", "8"))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// >   element(7, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4", "5", "6", "7", "8"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", "", "", "", "", ""))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// >   element(7, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
          all_errors(h),
        ]))
    }
  }
}

/// Decode 9 values from a `Dynamic` value.
///
/// ## Examples
///
/// ```gleam
/// > from(#(1, 2.1, "3", "4", "5", "6", "7", "8", "9"))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// >   element(7, string),
/// >   element(8, string),
/// > )
/// Ok(MyRecord(1, 2.1, "3", "4", "5", "6", "7", "8", "9"))
/// ```
///
/// ```gleam
/// > from(#("", "", "", "", "", "", "", "", ""))
/// > |> decode7(
/// >   MyRecord,
/// >   element(0, int),
/// >   element(1, float),
/// >   element(2, string),
/// >   element(3, string),
/// >   element(4, string),
/// >   element(5, string),
/// >   element(6, string),
/// >   element(7, string),
/// >   element(8, string),
/// > )
/// Error([
///   DecodeError(expected: "Int", found: "String", path: ["0"]),
///   DecodeError(expected: "Float", found: "String", path: ["1"]),
/// ])
/// ```
///
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
        Error(list.flatten([
          all_errors(a),
          all_errors(b),
          all_errors(c),
          all_errors(d),
          all_errors(e),
          all_errors(f),
          all_errors(g),
          all_errors(h),
          all_errors(i),
        ]))
    }
  }
}

fn all_errors(result: Result(a, List(DecodeError))) -> List(DecodeError) {
  case result {
    Ok(_) -> []
    Error(errors) -> errors
  }
}

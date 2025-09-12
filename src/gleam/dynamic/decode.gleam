//// The `Dynamic` type is used to represent dynamically typed data. That is, data
//// that we don't know the precise type of yet, so we need to introspect the data to
//// see if it is of the desired type before we can use it. Typically data like this
//// would come from user input or from untyped languages such as Erlang or JavaScript.
////
//// This module provides the `Decoder` type and associated functions, which provides
//// a type-safe and composable way to convert dynamic data into some desired type,
//// or into errors if the data doesn't have the desired structure.
////
//// The `Decoder` type is generic and has 1 type parameter, which is the type that
//// it attempts to decode. A `Decoder(String)` can be used to decode strings, and a
//// `Decoder(Option(Int))` can be used to decode `Option(Int)`s
////
//// Decoders work using _runtime reflection_ and the data structures of the target
//// platform. Differences between Erlang and JavaScript data structures may impact
//// your decoders, so it is important to test your decoders on all supported
//// platforms.
////
//// The decoding technique used by this module was inspired by Juraj Petráš'
//// [Toy](https://github.com/Hackder/toy), Go's `encoding/json`, and Elm's
//// `Json.Decode`. Thank you to them!
////
//// # Examples
////
//// Dynamic data may come from various sources and so many different syntaxes could
//// be used to describe or construct them. In these examples a pseudocode
//// syntax is used to describe the data.
////
//// ## Simple types
////
//// This module defines decoders for simple data types such as [`string`](#string),
//// [`int`](#int), [`float`](#float), [`bit_array`](#bit_array), and [`bool`](#bool).
////
//// ```gleam
//// // Data:
//// // "Hello, Joe!"
////
//// let result = decode.run(data, decode.string)
//// assert result == Ok("Hello, Joe!")
//// ```
////
//// ## Lists
////
//// The [`list`](#list) decoder decodes `List`s. To use it you must construct it by
//// passing in another decoder into the `list` function, which is the decoder that
//// is to be used for the elements of the list, type checking both the list and its
//// elements.
////
//// ```gleam
//// // Data:
//// // [1, 2, 3, 4]
////
//// let result = decode.run(data, decode.list(decode.int))
//// assert result == Ok([1, 2, 3, 4])
//// ```
////
//// On Erlang this decoder can decode from lists, and on JavaScript it can
//// decode from lists as well as JavaScript arrays.
////
//// ## Options
////
//// The [`optional`](#optional) decoder is used to decode values that may or may not
//// be present. In other environment these might be called "nullable" values.
////
//// Like the `list` decoder, the `optional` decoder takes another decoder,
//// which is used to decode the value if it is present.
////
//// ```gleam
//// // Data:
//// // 12.45
////
//// let result = decode.run(data, decode.optional(decode.float))
//// assert result == Ok(option.Some(12.45))
//// ```
//// ```gleam
//// // Data:
//// // null
////
//// let result = decode.run(data, decode.optional(decode.int))
//// assert result == Ok(option.None)
//// ```
////
//// This decoder knows how to handle multiple different runtime representations of
//// absent values, including `Nil`, `None`, `null`, and `undefined`.
////
//// ## Dicts
////
//// The [`dict`](#dict) decoder decodes `Dicts` and contains two other decoders, one
//// for the keys, one for the values.
////
//// ```gleam
//// // Data:
//// // { "Lucy" -> 10, "Nubi" -> 20 }
////
//// let result = decode.run(data, decode.dict(decode.string, decode.int))
//// assert result == Ok(dict.from_list([
////   #("Lucy", 10),
////   #("Nubi", 20),
//// ]))
//// ```
////
//// ## Indexing objects
////
//// The [`at`](#at) decoder can be used to decode a value that is nested within
//// key-value containers such as Gleam dicts, Erlang maps, or JavaScript objects.
////
//// ```gleam
//// // Data:
//// // { "one" -> { "two" -> 123 } }
////
//// let result = decode.run(data, decode.at(["one", "two"], decode.int))
//// assert result == Ok(123)
//// ```
////
//// ## Indexing arrays
////
//// If you use ints as keys then the [`at`](#at) decoder can be used to index into
//// array-like containers such as Gleam or Erlang tuples, or JavaScript arrays.
////
//// ```gleam
//// // Data:
//// // ["one", "two", "three"]
////
//// let result = decode.run(data, decode.at([1], decode.string))
//// assert result == Ok("two")
//// ```
////
//// ## Records
////
//// Decoding records from dynamic data is more complex and requires combining a
//// decoder for each field and a special constructor that builds your records with
//// the decoded field values.
////
//// ```gleam
//// // Data:
//// // {
//// //   "score" -> 180,
//// //   "name" -> "Mel Smith",
//// //   "is-admin" -> false,
//// //   "enrolled" -> true,
//// //   "colour" -> "Red",
//// // }
////
//// let decoder = {
////   use name <- decode.field("name", decode.string)
////   use score <- decode.field("score", decode.int)
////   use colour <- decode.field("colour", decode.string)
////   use enrolled <- decode.field("enrolled", decode.bool)
////   decode.success(Player(name:, score:, colour:, enrolled:))
//// }
////
//// let result = decode.run(data, decoder)
//// assert result == Ok(Player("Mel Smith", 180, "Red", True))
//// ```
////
//// ## Enum variants
////
//// Imagine you have a custom type where all the variants do not contain any values.
////
//// ```gleam
//// pub type PocketMonsterType {
////   Fire
////   Water
////   Grass
////   Electric
//// }
//// ```
////
//// You might choose to encode these variants as strings, `"fire"` for `Fire`,
//// `"water"` for `Water`, and so on. To decode them you'll need to decode the dynamic
//// data as a string, but then you'll need to decode it further still as not all
//// strings are valid values for the enum. This can be done with the `then`
//// function, which enables running a second decoder after the first one
//// succeeds.
////
//// ```gleam
//// let decoder = {
////   use decoded_string <- decode.then(decode.string)
////   case decoded_string {
////     // Return succeeding decoders for valid strings
////     "fire" -> decode.success(Fire)
////     "water" -> decode.success(Water)
////     "grass" -> decode.success(Grass)
////     "electric" -> decode.success(Electric)
////     // Return a failing decoder for any other strings
////     _ -> decode.failure(Fire, "PocketMonsterType")
////   }
//// }
////
//// let result = decode.run(dynamic.string("water"), decoder)
//// assert result == Ok(Water)
////
//// let result = decode.run(dynamic.string("wobble"), decoder)
//// assert result == Error([DecodeError("PocketMonsterType", "String", [])])
//// ```
////
//// ## Record variants
////
//// Decoding type variants that contain other values is done by combining the
//// techniques from the "enum variants" and "records" examples. Imagine you have
//// this custom type that you want to decode:
////
//// ```gleam
//// pub type PocketMonsterPerson {
////   Trainer(name: String, badge_count: Int)
////   GymLeader(name: String, speciality: PocketMonsterType)
//// }
//// ```
//// And you would like to be able to decode these from dynamic data like this:
//// ```erlang
//// {
////   "type" -> "trainer",
////   "name" -> "Ash",
////   "badge-count" -> 1,
//// }
//// ```
//// ```erlang
//// {
////   "type" -> "gym-leader",
////   "name" -> "Misty",
////   "speciality" -> "water",
//// }
//// ```
////
//// Notice how both documents have a `"type"` field, which is used to indicate which
//// variant the data is for.
////
//// First, define decoders for each of the variants:
////
//// ```gleam
//// let trainer_decoder = {
////   use name <- decode.field("name", decode.string)
////   use badge_count <- decode.field("badge-count", decode.int)
////   decode.success(Trainer(name, badge_count))
//// }
////
//// let gym_leader_decoder = {
////   use name <- decode.field("name", decode.string)
////   use speciality <- decode.field("speciality", pocket_monster_type_decoder)
////   decode.success(GymLeader(name, speciality))
//// }
//// ```
////
//// A third decoder can be used to extract and decode the `"type"` field, and the
//// expression can evaluate to whichever decoder is suitable for the document.
////
//// ```gleam
//// // Data:
//// // {
//// //   "type" -> "gym-leader",
//// //   "name" -> "Misty",
//// //   "speciality" -> "water",
//// // }
////
//// let decoder = {
////   use tag <- decode.field("type", decode.string)
////   case tag {
////     "gym-leader" -> gym_leader_decoder
////     _ -> trainer_decoder
////   }
//// }
////
//// let result = decode.run(data, decoder)
//// assert result == Ok(GymLeader("Misty", Water))
//// ```

import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// `Dynamic` data is data that we don't know the type of yet, originating from
/// external untyped systems.
///
/// You should never be converting your well typed data to dynamic data.
///
pub type Dynamic =
  dynamic.Dynamic

/// Error returned when unexpected data is encountered
///
pub type DecodeError {
  DecodeError(expected: String, found: String, path: List(String))
}

/// A decoder is a value that can be used to turn dynamically typed `Dynamic`
/// data into typed data using the `run` function.
///
/// Several smaller decoders can be combined to make larger decoders using
/// functions such as `list` and `field`.
///
pub opaque type Decoder(t) {
  Decoder(function: fn(Dynamic) -> #(t, List(DecodeError)))
}

/// The same as [`field`](#field), except taking a path to the value rather
/// than a field name.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.properties([
///   #(dynamic.string("data"), dynamic.properties([
///     #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///     #(dynamic.string("name"), dynamic.string("Lucy")),
///   ])
/// ]))
///
/// let decoder = {
///   use name <- decode.subfield(["data", "name"], decode.string)
///   use email <- decode.subfield(["data", "email"], decode.string)
///   decode.success(SignUp(name: name, email: email))
/// }
/// let result = decode.run(data, decoder)
/// assert result == Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn subfield(
  field_path: List(name),
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  Decoder(function: fn(data) {
    let #(out, errors1) =
      index(field_path, [], field_decoder.function, data, fn(data, position) {
        let #(default, _) = field_decoder.function(data)
        #(default, [DecodeError("Field", "Nothing", [])])
        |> push_path(list.reverse(position))
      })
    let #(out, errors2) = next(out).function(data)
    #(out, list.append(errors1, errors2))
  })
}

/// Run a decoder on a `Dynamic` value, decoding the value if it is of the
/// desired type, or returning errors.
///
/// # Examples
///
/// ```gleam
/// let decoder = {
///   use name <- decode.field("email", decode.string)
///   use email <- decode.field("password", decode.string)
///   decode.success(SignUp(name: name, email: email))
/// }
///
/// decode.run(data, decoder)
/// ```
///
pub fn run(data: Dynamic, decoder: Decoder(t)) -> Result(t, List(DecodeError)) {
  let #(maybe_invalid_data, errors) = decoder.function(data)
  case errors {
    [] -> Ok(maybe_invalid_data)
    [_, ..] -> Error(errors)
  }
}

/// A decoder that decodes a value that is nested within other values. For
/// example, decoding a value that is within some deeply nested JSON objects.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// # Examples
///
/// ```gleam
/// let decoder = decode.at(["one", "two"], decode.int)
///
/// let data = dynamic.properties([
///   #(dynamic.string("one"), dynamic.properties([
///     #(dynamic.string("two"), dynamic.int(1000)),
///   ])),
/// ]))
///
///
/// decode.run(data, decoder)
/// // -> Ok(1000)
/// ```
///
/// ```gleam
/// dynamic.nil()
/// |> decode.run(decode.optional(decode.int))
/// // -> Ok(option.None)
/// ```
///
pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  Decoder(function: fn(data) {
    index(path, [], inner.function, data, fn(data, position) {
      let #(default, _) = inner.function(data)
      #(default, [DecodeError("Field", "Nothing", [])])
      |> push_path(list.reverse(position))
    })
  })
}

fn index(
  path: List(a),
  position: List(a),
  inner: fn(Dynamic) -> #(b, List(DecodeError)),
  data: Dynamic,
  handle_miss: fn(Dynamic, List(a)) -> #(b, List(DecodeError)),
) -> #(b, List(DecodeError)) {
  case path {
    [] -> {
      data
      |> inner
      |> push_path(list.reverse(position))
    }

    [key, ..path] -> {
      case bare_index(data, key) {
        Ok(Some(data)) -> {
          index(path, [key, ..position], inner, data, handle_miss)
        }
        Ok(None) -> {
          handle_miss(data, [key, ..position])
        }
        Error(kind) -> {
          let #(default, _) = inner(data)
          #(default, [DecodeError(kind, dynamic.classify(data), [])])
          |> push_path(list.reverse(position))
        }
      }
    }
  }
}

@external(erlang, "gleam_stdlib", "index")
@external(javascript, "../../gleam_stdlib.mjs", "index")
fn bare_index(data: Dynamic, key: anything) -> Result(Option(Dynamic), String)

fn push_path(
  layer: #(t, List(DecodeError)),
  path: List(key),
) -> #(t, List(DecodeError)) {
  let decoder = one_of(string, [int |> map(int.to_string)])
  let path =
    list.map(path, fn(key) {
      let key = cast(key)
      case run(key, decoder) {
        Ok(key) -> key
        Error(_) -> "<" <> dynamic.classify(key) <> ">"
      }
    })
  let errors =
    list.map(layer.1, fn(error) {
      DecodeError(..error, path: list.append(path, error.path))
    })
  #(layer.0, errors)
}

/// Finalise a decoder having successfully extracted a value.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.properties([
///   #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///   #(dynamic.string("name"), dynamic.string("Lucy")),
/// ]))
///
/// let decoder = {
///   use name <- decode.field("name", string)
///   use email <- decode.field("email", string)
///   decode.success(SignUp(name: name, email: email))
/// }
///
/// let result = decode.run(data, decoder)
/// assert result == Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
pub fn success(data: t) -> Decoder(t) {
  Decoder(function: fn(_) { #(data, []) })
}

/// Construct a decode error for some unexpected dynamic data.
///
pub fn decode_error(
  expected expected: String,
  found found: Dynamic,
) -> List(DecodeError) {
  [DecodeError(expected: expected, found: dynamic.classify(found), path: [])]
}

/// Run a decoder on a field of a `Dynamic` value, decoding the value if it is
/// of the desired type, or returning errors. An error is returned if there is
/// no field for the specified key.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.properties([
///   #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///   #(dynamic.string("name"), dynamic.string("Lucy")),
/// ]))
///
/// let decoder = {
///   use name <- decode.field("name", string)
///   use email <- decode.field("email", string)
///   decode.success(SignUp(name: name, email: email))
/// }
///
/// let result = decode.run(data, decoder)
/// assert result == Ok(SignUp(name: "Lucy", email: "lucy@example.com"))
/// ```
///
/// If you wish to decode a value that is more deeply nested within the dynamic
/// data, see [`subfield`](#subfield) and [`at`](#at).
///
/// If you wish to return a default in the event that a field is not present,
/// see [`optional_field`](#optional_field) and / [`optionally_at`](#optionally_at).
///
pub fn field(
  field_name: name,
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  subfield([field_name], field_decoder, next)
}

/// Run a decoder on a field of a `Dynamic` value, decoding the value if it is
/// of the desired type, or returning errors. The given default value is
/// returned if there is no field for the specified key.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// # Examples
///
/// ```gleam
/// let data = dynamic.properties([
///   #(dynamic.string("name"), dynamic.string("Lucy")),
/// ]))
///
/// let decoder = {
///   use name <- decode.field("name", string)
///   use email <- decode.optional_field("email", "n/a", string)
///   decode.success(SignUp(name: name, email: email))
/// }
///
/// let result = decode.run(data, decoder)
/// assert result == Ok(SignUp(name: "Lucy", email: "n/a"))
/// ```
///
pub fn optional_field(
  key: name,
  default: t,
  field_decoder: Decoder(t),
  next: fn(t) -> Decoder(final),
) -> Decoder(final) {
  Decoder(function: fn(data) {
    let #(out, errors1) =
      case bare_index(data, key) {
        Ok(Some(data)) -> field_decoder.function(data)
        Ok(None) -> #(default, [])
        Error(kind) -> #(default, [
          DecodeError(kind, dynamic.classify(data), []),
        ])
      }
      |> push_path([key])
    let #(out, errors2) = next(out).function(data)
    #(out, list.append(errors1, errors2))
  })
}

/// A decoder that decodes a value that is nested within other values. For
/// example, decoding a value that is within some deeply nested JSON objects.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// # Examples
///
/// ```gleam
/// let decoder = decode.optionally_at(["one", "two"], 100, decode.int)
///
/// let data = dynamic.properties([
///   #(dynamic.string("one"), dynamic.properties([])),
/// ]))
///
///
/// decode.run(data, decoder)
/// // -> Ok(100)
/// ```
///
pub fn optionally_at(
  path: List(segment),
  default: a,
  inner: Decoder(a),
) -> Decoder(a) {
  Decoder(function: fn(data) {
    index(path, [], inner.function, data, fn(_, _) { #(default, []) })
  })
}

fn run_dynamic_function(
  data: Dynamic,
  name: String,
  f: fn(Dynamic) -> Result(t, t),
) -> #(t, List(DecodeError)) {
  case f(data) {
    Ok(data) -> #(data, [])
    Error(zero) -> #(zero, [DecodeError(name, dynamic.classify(data), [])])
  }
}

/// A decoder that decodes `String` values.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.string("Hello!"), decode.string)
/// assert result == Ok("Hello!")
/// ```
///
pub const string: Decoder(String) = Decoder(decode_string)

fn decode_string(data: Dynamic) -> #(String, List(DecodeError)) {
  run_dynamic_function(data, "String", dynamic_string)
}

@external(javascript, "../../gleam_stdlib.mjs", "string")
fn dynamic_string(from data: Dynamic) -> Result(String, String) {
  case dynamic_bit_array(data) {
    Ok(data) ->
      case bit_array.to_string(data) {
        Ok(string) -> Ok(string)
        Error(_) -> Error("")
      }
    Error(_) -> Error("")
  }
}

/// A decoder that decodes `Bool` values.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.bool(True), decode.bool)
/// assert result == Ok(True)
/// ```
///
pub const bool: Decoder(Bool) = Decoder(decode_bool)

fn decode_bool(data: Dynamic) -> #(Bool, List(DecodeError)) {
  case cast(True) == data {
    True -> #(True, [])
    False ->
      case cast(False) == data {
        True -> #(False, [])
        False -> #(False, decode_error("Bool", data))
      }
  }
}

/// A decoder that decodes `Int` values.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.int(147), decode.int)
/// assert result == Ok(147)
/// ```
///
pub const int: Decoder(Int) = Decoder(decode_int)

fn decode_int(data: Dynamic) -> #(Int, List(DecodeError)) {
  run_dynamic_function(data, "Int", dynamic_int)
}

@external(erlang, "gleam_stdlib", "int")
@external(javascript, "../../gleam_stdlib.mjs", "int")
fn dynamic_int(data: Dynamic) -> Result(Int, Int)

/// A decoder that decodes `Float` values.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.float(3.14), decode.float)
/// assert result == Ok(3.14)
/// ```
///
pub const float: Decoder(Float) = Decoder(decode_float)

fn decode_float(data: Dynamic) -> #(Float, List(DecodeError)) {
  run_dynamic_function(data, "Float", dynamic_float)
}

@external(erlang, "gleam_stdlib", "float")
@external(javascript, "../../gleam_stdlib.mjs", "float")
fn dynamic_float(data: Dynamic) -> Result(Float, Float)

/// A decoder that decodes `Dynamic` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.float(3.14), decode.dynamic)
/// assert result == Ok(dynamic.float(3.14))
/// ```
///
pub const dynamic: Decoder(Dynamic) = Decoder(decode_dynamic)

fn decode_dynamic(data: Dynamic) -> #(Dynamic, List(DecodeError)) {
  #(data, [])
}

/// A decoder that decodes `BitArray` values. This decoder never returns an error.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.bit_array(<<5, 7>>), decode.bit_array)
/// assert result == Ok(<<5, 7>>)
/// ```
///
pub const bit_array: Decoder(BitArray) = Decoder(decode_bit_array)

fn decode_bit_array(data: Dynamic) -> #(BitArray, List(DecodeError)) {
  run_dynamic_function(data, "BitArray", dynamic_bit_array)
}

@external(erlang, "gleam_stdlib", "bit_array")
@external(javascript, "../../gleam_stdlib.mjs", "bit_array")
fn dynamic_bit_array(data: Dynamic) -> Result(BitArray, BitArray)

/// A decoder that decodes lists where all elements are decoded with a given
/// decoder.
///
/// # Examples
///
/// ```gleam
/// let result =
///   [1, 2, 3]
///   |> list.map(dynamic.int)
///   |> dynamic.list
///   |> decode.run(decode.list(of: decode.int))
/// assert result == Ok([1, 2, 3])
/// ```
///
pub fn list(of inner: Decoder(a)) -> Decoder(List(a)) {
  Decoder(fn(data) {
    decode_list(data, inner.function, fn(p, k) { push_path(p, [k]) }, 0, [])
  })
}

@external(erlang, "gleam_stdlib", "list")
@external(javascript, "../../gleam_stdlib.mjs", "list")
fn decode_list(
  data: Dynamic,
  item: fn(Dynamic) -> #(t, List(DecodeError)),
  push_path: fn(#(t, List(DecodeError)), key) -> #(t, List(DecodeError)),
  index: Int,
  acc: List(t),
) -> #(List(t), List(DecodeError))

/// A decoder that decodes dicts where all keys and vales are decoded with
/// given decoders.
///
/// # Examples
///
/// ```gleam
/// let values = dynamic.properties([
///   #(dynamic.string("one"), dynamic.int(1)),
///   #(dynamic.string("two"), dynamic.int(2)),
/// ])
///
/// let result =
///   decode.run(values, decode.dict(decode.string, decode.int))
/// assert result == Ok(values)
/// ```
///
pub fn dict(
  key: Decoder(key),
  value: Decoder(value),
) -> Decoder(Dict(key, value)) {
  Decoder(fn(data) {
    case decode_dict(data) {
      Error(_) -> #(dict.new(), decode_error("Dict", data))
      Ok(dict) ->
        dict.fold(dict, #(dict.new(), []), fn(a, k, v) {
          // If there are any errors from previous key-value pairs then we
          // don't need to run the decoders, instead return the existing acc.
          case a.1 {
            [] -> fold_dict(a, k, v, key.function, value.function)
            [_, ..] -> a
          }
        })
    }
  })
}

fn fold_dict(
  acc: #(Dict(k, v), List(DecodeError)),
  key: Dynamic,
  value: Dynamic,
  key_decoder: fn(Dynamic) -> #(k, List(DecodeError)),
  value_decoder: fn(Dynamic) -> #(v, List(DecodeError)),
) -> #(Dict(k, v), List(DecodeError)) {
  // First we decode the key.
  case key_decoder(key) {
    #(key, []) ->
      // Then we decode the value.
      case value_decoder(value) {
        #(value, []) -> {
          // It worked! Insert the new key-value pair so we can move onto the next.
          let dict = dict.insert(acc.0, key, value)
          #(dict, acc.1)
        }
        #(_, errors) -> push_path(#(dict.new(), errors), ["values"])
      }
    #(_, errors) -> push_path(#(dict.new(), errors), ["keys"])
  }
}

@external(erlang, "gleam_stdlib", "dict")
@external(javascript, "../../gleam_stdlib.mjs", "dict")
fn decode_dict(data: Dynamic) -> Result(Dict(Dynamic, Dynamic), Nil)

/// A decoder that decodes nullable values of a type decoded by with a given
/// decoder.
///
/// This function can handle common representations of null on all runtimes, such as
/// `nil`, `null`, and `undefined` on Erlang, and `undefined` and `null` on
/// JavaScript.
///
/// # Examples
///
/// ```gleam
/// let result = decode.run(dynamic.int(100), decode.optional(decode.int))
/// assert result == Ok(option.Some(100))
/// ```
///
/// ```gleam
/// let result = decode.run(dynamic.nil(), decode.optional(decode.int))
/// assert result == Ok(option.None)
/// ```
///
pub fn optional(inner: Decoder(a)) -> Decoder(Option(a)) {
  Decoder(function: fn(data) {
    case is_null(data) {
      True -> #(option.None, [])
      False -> {
        let #(data, errors) = inner.function(data)
        #(option.Some(data), errors)
      }
    }
  })
}

/// Apply a transformation function to any value decoded by the decoder.
///
/// # Examples
///
/// ```gleam
/// let decoder = decode.int |> decode.map(int.to_string)
/// let result = decode.run(dynamic.int(1000), decoder)
/// assert result == Ok("1000")
/// ```
///
pub fn map(decoder: Decoder(a), transformer: fn(a) -> b) -> Decoder(b) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(transformer(data), errors)
  })
}

/// Apply a transformation function to any errors returned by the decoder.
///
pub fn map_errors(
  decoder: Decoder(a),
  transformer: fn(List(DecodeError)) -> List(DecodeError),
) -> Decoder(a) {
  Decoder(function: fn(d) {
    let #(data, errors) = decoder.function(d)
    #(data, transformer(errors))
  })
}

/// Replace all errors produced by a decoder with one single error for a named
/// expected type.
///
/// This function may be useful if you wish to simplify errors before
/// presenting them to a user, particularly when using the `one_of` function.
///
/// # Examples
///
/// ```gleam
/// let decoder = decode.string |> decode.collapse_errors("MyThing")
/// let result = decode.run(dynamic.int(1000), decoder)
/// assert result == Error([DecodeError("MyThing", "Int", [])])
/// ```
///
pub fn collapse_errors(decoder: Decoder(a), name: String) -> Decoder(a) {
  Decoder(function: fn(dynamic_data) {
    let #(data, errors) as layer = decoder.function(dynamic_data)
    case errors {
      [] -> layer
      [_, ..] -> #(data, decode_error(name, dynamic_data))
    }
  })
}

/// Create a new decoder based upon the value of a previous decoder.
///
/// This may be useful to run one previous decoder to use in further decoding.
///
pub fn then(decoder: Decoder(a), next: fn(a) -> Decoder(b)) -> Decoder(b) {
  Decoder(function: fn(dynamic_data) {
    let #(data, errors) = decoder.function(dynamic_data)
    let decoder = next(data)
    let #(data, _) as layer = decoder.function(dynamic_data)
    case errors {
      [] -> layer
      [_, ..] -> #(data, errors)
    }
  })
}

/// Create a new decoder from several other decoders. Each of the inner
/// decoders is run in turn, and the value from the first to succeed is used.
///
/// If no decoder succeeds then the errors from the first decoder is used.
/// If you wish for different errors then you may wish to use the
/// `collapse_errors` or `map_errors` functions.
///
/// # Examples
///
/// ```gleam
/// let decoder = decode.one_of(decode.string, or: [
///   decode.int |> decode.map(int.to_string),
///   decode.float |> decode.map(float.to_string),
/// ])
/// decode.run(dynamic.int(1000), decoder)
/// // -> Ok("1000")
/// ```
///
pub fn one_of(
  first: Decoder(a),
  or alternatives: List(Decoder(a)),
) -> Decoder(a) {
  Decoder(function: fn(dynamic_data) {
    let #(_, errors) as layer = first.function(dynamic_data)
    case errors {
      [] -> layer
      [_, ..] -> run_decoders(dynamic_data, layer, alternatives)
    }
  })
}

fn run_decoders(
  data: Dynamic,
  failure: #(a, List(DecodeError)),
  decoders: List(Decoder(a)),
) -> #(a, List(DecodeError)) {
  case decoders {
    [] -> failure

    [decoder, ..decoders] -> {
      let #(_, errors) as layer = decoder.function(data)
      case errors {
        [] -> layer
        [_, ..] -> run_decoders(data, failure, decoders)
      }
    }
  }
}

/// Define a decoder that always fails. The parameter for this function is the
/// name of the type that has failed to decode.
///
pub fn failure(zero: a, expected: String) -> Decoder(a) {
  Decoder(function: fn(d) { #(zero, decode_error(expected, d)) })
}

/// Create a decoder for a new data type from a decoding function.
///
/// This function is used for new primitive types. For example, you might
/// define a decoder for Erlang's pid type.
///
/// A default "zero" value is also required to make a decoder. When this
/// decoder is used as part of a larger decoder this zero value used as
/// a placeholder so that the rest of the decoder can continue to run and
/// collect all decoding errors.
///
/// If you were to make a decoder for the `String` type (rather than using the
/// build-in `string` decoder) you would define it like so:
///
/// ```gleam
/// pub fn string_decoder() -> decode.Decoder(String) {
///   let default = ""
///   decode.new_primitive_decoder("String", fn(data) {
///     case dynamic.string(data) {
///       Ok(x) -> Ok(x)
///       Error(_) -> Error(default)
///     }
///   })
/// }
/// ```
///
pub fn new_primitive_decoder(
  name: String,
  decoding_function: fn(Dynamic) -> Result(t, t),
) -> Decoder(t) {
  Decoder(function: fn(d) {
    case decoding_function(d) {
      Ok(t) -> #(t, [])
      Error(zero) -> #(zero, [DecodeError(name, dynamic.classify(d), [])])
    }
  })
}

/// Create a decoder that can refer to itself, useful for decoding deeply
/// nested data.
///
/// Attempting to create a recursive decoder without this function could result
/// in an infinite loop. If you are using `field` or other `use`able functions
/// then you may not need to use this function.
///
/// ```gleam
/// type Nested {
///   Nested(List(Nested))
///   Value(String)
/// }
///
/// fn nested_decoder() -> decode.Decoder(Nested) {
///   use <- decode.recursive
///   decode.one_of(decode.string |> decode.map(Value), [
///     decode.list(nested_decoder()) |> decode.map(Nested),
///   ])
/// }
/// ```
///
pub fn recursive(inner: fn() -> Decoder(a)) -> Decoder(a) {
  Decoder(function: fn(data) {
    let decoder = inner()
    decoder.function(data)
  })
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../../gleam_stdlib.mjs", "identity")
fn cast(a: anything) -> Dynamic

@external(erlang, "gleam_stdlib", "is_null")
@external(javascript, "../../gleam_stdlib.mjs", "is_null")
fn is_null(a: Dynamic) -> Bool

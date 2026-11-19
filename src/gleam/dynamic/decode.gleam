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
//// # Generating decoders
////
//// The language server has the "generate dynamic decoder" code action, which
//// will generate a decoder function when run on a custom type definition.
//// This generated decoder function can be a convenient shortcut when creating
//// your own decoders, and you can edit the generated function to suit your needs.
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
//// be present. In other environments these might be called "nullable" values.
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
//// assert result
////   == Ok(
////     dict.from_list([
////       #("Lucy", 10),
////       #("Nubi", 20),
////     ]),
////   )
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
////     _ -> decode.failure(Fire, expected: "PocketMonsterType")
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
import gleam/list
import gleam/option.{type Option}

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

/// Field lookups can result in 4 outcomes: The value indexed and the key could
/// be found, the value could be indexed but the field was missing, or the value
/// could not be indexed either by an integer or some other key.
@internal
pub type Lookup {
  Found(value: Dynamic)
  Missing
  InvalidIndex
  InvalidKey
}

/// The same as [`field`](#field), except taking a path to the value rather
/// than a field name.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// ## Examples
///
/// ```gleam
/// let data =
///   dynamic.properties([
///     #(
///       dynamic.string("data"),
///       dynamic.properties([
///         #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///         #(dynamic.string("name"), dynamic.string("Lucy")),
///       ]),
///     ),
///   ])
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
  let Decoder(function: inner) = at(field_path, field_decoder)
  Decoder(function: fn(data) {
    case inner(data) {
      #(value, []) -> next(value).function(data)
      #(value, errors) -> continue_with_errors(next, data, value, errors)
    }
  })
}

/// Run a decoder on a `Dynamic` value, decoding the value if it is of the
/// desired type, or returning errors.
///
/// ## Examples
///
/// ```gleam
/// let decoder = {
///   use name <- decode.field("name", decode.string)
///   use email <- decode.field("email", decode.string)
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
/// ## Examples
///
/// ```gleam
/// let decoder = decode.at(["one", "two"], decode.int)
///
/// let data =
///   dynamic.properties([
///     #(
///       dynamic.string("one"),
///       dynamic.properties([
///         #(dynamic.string("two"), dynamic.int(1000)),
///       ]),
///     ),
///   ])
///
/// assert decode.run(data, decoder) == Ok(1000)
/// ```
///
/// ```gleam
/// assert dynamic.nil()
///   |> decode.run(decode.optional(decode.int))
///   == Ok(option.None)
/// ```
///
pub fn at(path: List(segment), inner: Decoder(a)) -> Decoder(a) {
  let decode_inner = inner.function

  let step = fn(inner, key) {
    fn(data) {
      case lookup_field(data, key) {
        Found(child) ->
          case inner(child) {
            #(_, []) as result -> result
            #(value, errors) -> #(value, prefix_errors(key, errors))
          }

        Missing -> #(decode_inner(data).0, missing_field_error(key))
        InvalidIndex -> #(decode_inner(data).0, decode_error("Indexable", data))
        InvalidKey -> #(decode_inner(data).0, decode_error("Dict", data))
      }
    }
  }

  Decoder(function: list.fold_right(over: path, from: decode_inner, with: step))
}

@external(erlang, "gleam_stdlib", "lookup_field")
@external(javascript, "../../gleam_stdlib.mjs", "lookup_field")
fn lookup_field(data: Dynamic, name: name) -> Lookup

@external(erlang, "gleam_stdlib", "key_to_string")
@external(javascript, "../../gleam_stdlib.mjs", "key_to_string")
fn key_to_string(key: anything) -> String

fn prefix_errors(
  key: anything,
  errors: List(DecodeError),
) -> List(DecodeError) {
  let key = key_to_string(key)
  list.map(errors, fn(error) { DecodeError(..error, path: [key, ..error.path]) })
}

fn missing_field_error(key: anything) -> List(DecodeError) {
  [DecodeError("Field", "Nothing", [key_to_string(key)])]
}

fn continue_with_errors(
  next: fn(a) -> Decoder(t),
  data: Dynamic,
  field: a,
  earlier: List(DecodeError),
) -> #(t, List(DecodeError)) {
  case next(field).function(data) {
    #(value, []) -> #(value, earlier)
    #(value, later) -> #(value, list.append(earlier, later))
  }
}

/// Finalise a decoder having successfully extracted a value.
///
/// ## Examples
///
/// ```gleam
/// let data =
///   dynamic.properties([
///     #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///     #(dynamic.string("name"), dynamic.string("Lucy")),
///   ])
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
/// ## Examples
///
/// ```gleam
/// let data =
///   dynamic.properties([
///     #(dynamic.string("email"), dynamic.string("lucy@example.com")),
///     #(dynamic.string("name"), dynamic.string("Lucy")),
///   ])
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
  let decode_field = field_decoder.function

  Decoder(function: fn(data) {
    let lookup_error = fn(errors) {
      continue_with_errors(next, data, decode_field(data).0, errors)
    }

    case lookup_field(data, field_name) {
      Found(child) ->
        case decode_field(child) {
          #(value, []) -> next(value).function(data)
          #(value, errors) -> {
            let errors = prefix_errors(field_name, errors)
            continue_with_errors(next, data, value, errors)
          }
        }
      Missing -> lookup_error(missing_field_error(field_name))
      InvalidIndex -> lookup_error(decode_error("Indexable", data))
      InvalidKey -> lookup_error(decode_error("Dict", data))
    }
  })
}

/// Run a decoder on a field of a `Dynamic` value, decoding the value if it is
/// of the desired type, or returning errors. The given default value is
/// returned if there is no field for the specified key.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// ## Examples
///
/// ```gleam
/// let data =
///   dynamic.properties([
///     #(dynamic.string("name"), dynamic.string("Lucy")),
///   ])
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
  let decode_field = field_decoder.function

  Decoder(function: fn(data) {
    case optional_step(data, key, default, decode_field) {
      #(value, []) -> next(value).function(data)
      #(value, errors) -> continue_with_errors(next, data, value, errors)
    }
  })
}

/// A decoder that decodes a value that is nested within other values. For
/// example, decoding a value that is within some deeply nested JSON objects.
///
/// This function will index into dictionaries with any key type, and if the key is
/// an int then it'll also index into Erlang tuples and JavaScript arrays, and
/// the first eight elements of Gleam lists.
///
/// ## Examples
///
/// ```gleam
/// let decoder = decode.optionally_at(["one", "two"], 100, decode.int)
///
/// let data =
///   dynamic.properties([
///     #(dynamic.string("one"), dynamic.properties([])),
///   ])
///
/// assert decode.run(data, decoder) == Ok(100)
/// ```
///
pub fn optionally_at(
  path: List(segment),
  default: a,
  inner: Decoder(a),
) -> Decoder(a) {
  let function =
    list.fold_right(over: path, from: inner.function, with: fn(inner, key) {
      fn(data) { optional_step(data, key, default, inner) }
    })

  Decoder(function:)
}

fn optional_step(
  data: Dynamic,
  key: key,
  default: a,
  inner: fn(Dynamic) -> #(a, List(DecodeError)),
) -> #(a, List(DecodeError)) {
  case lookup_field(data, key) {
    Found(child) ->
      case inner(child) {
        #(_, []) as result -> result
        #(value, errors) -> #(value, prefix_errors(key, errors))
      }
    Missing -> #(default, [])
    InvalidIndex -> #(default, decode_error("Indexable", data))
    InvalidKey -> #(default, decode_error("Dict", data))
  }
}

/// A decoder that decodes `String` values.
///
/// ## Examples
///
/// ```gleam
/// let result = decode.run(dynamic.string("Hello!"), decode.string)
/// assert result == Ok("Hello!")
/// ```
///
pub const string: Decoder(String) = Decoder(decode_string)

fn decode_string(data: Dynamic) -> #(String, List(DecodeError)) {
  case is_string(data) {
    True -> #(cast_from_dynamic(data), [])
    False -> #("", decode_error("String", data))
  }
}

@external(javascript, "../../gleam_stdlib.mjs", "is_string")
fn is_string(data: Dynamic) -> Bool {
  is_bit_array(data) && bit_array.is_utf8(cast_from_dynamic(data))
}

/// A decoder that decodes `Bool` values.
///
/// ## Examples
///
/// ```gleam
/// let result = decode.run(dynamic.bool(True), decode.bool)
/// assert result == Ok(True)
/// ```
///
pub const bool: Decoder(Bool) = Decoder(decode_bool)

fn decode_bool(data: Dynamic) -> #(Bool, List(DecodeError)) {
  case is_bool(data) {
    True -> #(cast_from_dynamic(data), [])
    False -> #(False, decode_error("Bool", data))
  }
}

@external(erlang, "erlang", "is_boolean")
@external(javascript, "../../gleam_stdlib.mjs", "is_bool")
fn is_bool(data: Dynamic) -> Bool

/// A decoder that decodes `Int` values.
///
/// This will not coerse float values into int values, so on platforms with
/// distinct runtime int and float types (Erlang, not JavaScript) it will fail,
/// even if the float is a whole number (e.g. 1.0).
///
/// If you want to decode both ints and floats you may want to use the `one_of`
/// function.
///
/// ## Examples
///
/// ```gleam
/// let result = decode.run(dynamic.int(147), decode.int)
/// assert result == Ok(147)
/// ```
///
pub const int: Decoder(Int) = Decoder(decode_int)

fn decode_int(data: Dynamic) -> #(Int, List(DecodeError)) {
  case is_int(data) {
    True -> #(cast_from_dynamic(data), [])
    False -> #(0, decode_error("Int", data))
  }
}

@external(erlang, "erlang", "is_integer")
@external(javascript, "../../gleam_stdlib.mjs", "is_int")
fn is_int(data: Dynamic) -> Bool

/// A decoder that decodes `Float` values.
///
/// This will not coerse int values into float values, so on platforms with
/// distinct runtime int and float types (Erlang, not JavaScript) it will fail
/// for ints. One time this may happen is when decoding JSON data.
///
/// If you want to decode both ints and floats you may want to use the `one_of`
/// function.
///
/// ## Examples
///
/// ```gleam
/// let result = decode.run(dynamic.float(3.14), decode.float)
/// assert result == Ok(3.14)
/// ```
///
pub const float: Decoder(Float) = Decoder(decode_float)

fn decode_float(data: Dynamic) -> #(Float, List(DecodeError)) {
  case is_float(data) {
    True -> #(cast_from_dynamic(data), [])
    False -> #(0.0, decode_error("Float", data))
  }
}

@external(erlang, "erlang", "is_float")
@external(javascript, "../../gleam_stdlib.mjs", "is_float")
fn is_float(data: Dynamic) -> Bool

/// A decoder that decodes `Dynamic` values. This decoder never returns an error.
///
/// ## Examples
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

/// A decoder that decodes `BitArray` values.
///
/// ## Examples
///
/// ```gleam
/// let result = decode.run(dynamic.bit_array(<<5, 7>>), decode.bit_array)
/// assert result == Ok(<<5, 7>>)
/// ```
///
pub const bit_array: Decoder(BitArray) = Decoder(decode_bit_array)

@external(javascript, "../../gleam_stdlib.mjs", "decode_bit_array")
fn decode_bit_array(data: Dynamic) -> #(BitArray, List(DecodeError)) {
  case is_bit_array(data) {
    True -> #(cast_from_dynamic(data), [])
    False -> #(<<>>, decode_error("BitArray", data))
  }
}

@external(erlang, "erlang", "is_bitstring")
fn is_bit_array(data: Dynamic) -> Bool

/// A decoder that decodes lists where all elements are decoded with a given
/// decoder.
///
/// ## Examples
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
  let decode_element = inner.function
  Decoder(function: decode_list(_, decode_element, prefix_errors))
}

@external(erlang, "gleam_stdlib", "decode_list")
@external(javascript, "../../gleam_stdlib.mjs", "decode_list")
fn decode_list(
  data: Dynamic,
  inner: fn(Dynamic) -> #(a, List(DecodeError)),
  prefix_errors: fn(String, List(DecodeError)) -> List(DecodeError),
) -> #(List(a), List(DecodeError))

/// A decoder that decodes dicts where all keys and values are decoded with
/// given decoders.
///
/// ## Examples
///
/// ```gleam
/// let values =
///   dynamic.properties([
///     #(dynamic.string("one"), dynamic.int(1)),
///     #(dynamic.string("two"), dynamic.int(2)),
///   ])
///
/// let result = decode.run(values, decode.dict(decode.string, decode.int))
/// assert result == Ok(values)
/// ```
///
pub fn dict(
  key: Decoder(key),
  value: Decoder(value),
) -> Decoder(Dict(key, value)) {
  let decode_key = key.function
  let decode_value = value.function

  let decode_entry = fn(acc: Dict(key, value), key: Dynamic, value: Dynamic) {
    case decode_key(key) {
      #(decoded_key, []) ->
        case decode_value(value) {
          #(value, []) -> #(dict.insert(acc, decoded_key, value), [])
          #(_, errors) -> #(dict.new(), prefix_errors(key, errors))
        }

      #(_, errors) -> #(dict.new(), prefix_errors("keys", errors))
    }
  }

  Decoder(function: decode_dict(_, decode_entry))
}

@external(erlang, "gleam_stdlib", "decode_dict")
@external(javascript, "../../gleam_stdlib.mjs", "decode_dict")
fn decode_dict(
  data: Dynamic,
  decode: fn(Dict(k, v), Dynamic, Dynamic) -> #(Dict(k, v), List(DecodeError)),
) -> #(Dict(k, v), List(DecodeError))

/// A decoder that decodes nullable values of a type decoded by with a given
/// decoder.
///
/// This function can handle common representations of null on all runtimes, such as
/// `nil`, `null`, and `undefined` on Erlang, and `undefined` and `null` on
/// JavaScript.
///
/// ## Examples
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
  let decode_value = inner.function
  Decoder(function: fn(data) {
    case is_null(data) {
      True -> #(option.None, [])
      False -> {
        let #(data, errors) = decode_value(data)
        #(option.Some(data), errors)
      }
    }
  })
}

/// Apply a transformation function to any value decoded by the decoder.
///
/// ## Examples
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
/// ## Examples
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
/// If no decoder succeeds then the errors from the first decoder are used.
/// If you wish for different errors then you may wish to use the
/// `collapse_errors` or `map_errors` functions.
///
/// ## Examples
///
/// ```gleam
/// let decoder =
///   decode.one_of(decode.string, or: [
///     decode.int |> decode.map(int.to_string),
///     decode.float |> decode.map(float.to_string),
///   ])
/// assert decode.run(dynamic.int(1000), decoder) == Ok("1000")
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

/// Define a decoder that always fails.
///
/// The first parameter is a "placeholder" value, which is some default value that the
/// decoder uses internally in place of the value that would have been produced
/// if the decoder was successful. It doesn't matter what this value is, it is
/// never returned by the decoder or shown to the user, so pick some arbitrary
/// value. If it is an int you might pick `0`, if it is a list you might pick
/// `[]`.
///
/// The second parameter is the name of the type that has failed to decode.
///
/// ```gleam
/// decode.failure(User(name: "", score: 0, tags: []), expected: "User")
/// ```
///
pub fn failure(placeholder: a, expected name: String) -> Decoder(a) {
  Decoder(function: fn(d) { #(placeholder, decode_error(name, d)) })
}

/// Create a decoder for a new data type from a decoding function.
///
/// This function is used for new primitive types. For example, you might
/// define a decoder for Erlang's pid type.
///
/// A default "placeholder" value is also required to make a decoder. When this
/// decoder is used as part of a larger decoder this placeholder value is used
/// so that the rest of the decoder can continue to run and
/// collect all decoding errors. It doesn't matter what this value is, it is
/// never returned by the decoder or shown to the user, so pick some arbitrary
/// value. If it is an int you might pick `0`, if it is a list you might pick
/// `[]`.
///
/// If you were to make a decoder for the `Int` type (rather than using the
/// built-in `Int` decoder) you would define it like so:
///
/// ```gleam
/// import gleam/dynamic.{type Dynamic}
/// import gleam/dynamic/decode.{type Decoder}
///
/// pub fn int_decoder() -> Decoder(Int) {
///   decode.new_primitive_decoder("Int", int_from_dynamic)
/// }
///
/// @external(erlang, "my_module", "int_from_dynamic")
/// fn int_from_dynamic(data: Dynamic) -> Result(Int, Int)
/// ```
///
/// ```erlang
/// -module(my_module).
/// -export([int_from_dynamic/1]).
///
/// int_from_dynamic(Data) ->
///     case is_integer(Data) of
///         true -> {ok, Data};
///         false -> {error, 0}
///     end.
/// ```
///
pub fn new_primitive_decoder(
  name: String,
  decoding_function: fn(Dynamic) -> Result(t, t),
) -> Decoder(t) {
  Decoder(function: fn(d) {
    case decoding_function(d) {
      Ok(t) -> #(t, [])
      Error(placeholder) -> #(placeholder, decode_error(name, d))
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
/// ## Examples
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

@external(erlang, "gleam_stdlib", "is_null")
@external(javascript, "../../gleam_stdlib.mjs", "is_null")
fn is_null(a: Dynamic) -> Bool

/// We use a local identity function here and refer to itself with @external to
/// erase it's type. This causes the Gleam compiler to emit a local call instead
/// of a remote function call, which causes the JIT to completely eliminate this
/// function call.
@external(erlang, "gleam/dynamic/decode", "identity")
@external(javascript, "./decode.mjs", "identity")
fn cast_from_dynamic(data: Dynamic) -> t

@internal
pub fn identity(value: anything) -> anything {
  value
}

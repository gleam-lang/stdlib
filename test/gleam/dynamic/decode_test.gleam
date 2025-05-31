import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{DecodeError}
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

pub type User {
  User(
    name: String,
    email: String,
    is_admin: Bool,
    is_confirmed: Bool,
    score: Int,
  )
}

pub fn decoder_test() {
  let data =
    dynamic.properties([
      #(dynamic.string("name"), dynamic.string("Nubi")),
      #(dynamic.string("email"), dynamic.string("nubi@example.com")),
      #(dynamic.string("is_admin"), dynamic.bool(False)),
      #(dynamic.string("is_confirmed"), dynamic.bool(True)),
      #(dynamic.string("score"), dynamic.int(180)),
    ])

  let decoder = {
    use name <- decode.field("name", decode.string)
    use email <- decode.field("email", decode.string)
    use is_admin <- decode.field("is_admin", decode.bool)
    use is_confirmed <- decode.field("is_confirmed", decode.bool)
    use score <- decode.field("score", decode.int)
    decode.success(User(
      name: name,
      email: email,
      is_admin: is_admin,
      is_confirmed: is_confirmed,
      score: score,
    ))
  }

  let assert Ok(value) = decode.run(data, decoder)
  assert value == User("Nubi", "nubi@example.com", False, True, 180)
}

pub fn field_ok_test() {
  let data =
    dynamic.properties([#(dynamic.string("name"), dynamic.string("Nubi"))])
  let decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }

  let assert Ok(value) = decode.run(data, decoder)
  assert value == "Nubi"
}

pub fn subfield_ok_test() {
  let data =
    dynamic.properties([
      #(
        dynamic.string("person"),
        dynamic.properties([#(dynamic.string("name"), dynamic.string("Nubi"))]),
      ),
    ])
  let decoder = {
    use name <- decode.subfield(["person", "name"], decode.string)
    decode.success(name)
  }

  let assert Ok(value) = decode.run(data, decoder)
  assert value == "Nubi"
}

pub fn field_int_index_ok_test() {
  let decoder = {
    use x <- decode.field(0, decode.string)
    use y <- decode.field(1, decode.string)
    decode.success(#(x, y))
  }

  let assert Ok(value) =
    dynamic.array(["one", "two", "three"] |> list.map(dynamic.string))
    |> decode.run(decoder)
  assert value == #("one", "two")
}

pub fn field_int_index_list_ok_test() {
  let decoder = {
    use x <- decode.field(0, decode.string)
    use y <- decode.field(1, decode.string)
    decode.success(#(x, y))
  }

  let assert Ok(value) =
    dynamic.list(["one", "two", "three", "four"] |> list.map(dynamic.string))
    |> decode.run(decoder)
  assert value == #("one", "two")
}

pub fn field_int_index_big_list_ok_test() {
  let decoder = {
    use x <- decode.field(6, decode.string)
    use y <- decode.field(7, decode.string)
    decode.success(#(x, y))
  }

  let assert Ok(value) =
    dynamic.list(
      ["one", "two", "three", "four", "five", "six", "seven", "eight"]
      |> list.map(dynamic.string),
    )
    |> decode.run(decoder)
  assert value == #("seven", "eight")
}

pub fn subfield_not_found_error_test() {
  let decoder = {
    use name <- decode.subfield(["name"], decode.string)
    decode.success(name)
  }

  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decoder)
  assert value == [DecodeError("Dict", "Int", [])]
}

pub fn field_not_found_error_test() {
  let decoder = {
    use name <- decode.subfield(["name"], decode.string)
    decode.success(name)
  }

  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decoder)
  assert value == [DecodeError("Dict", "Int", [])]
}

pub fn field_wrong_inner_error_test() {
  let decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }

  let assert Error(value) =
    dynamic.properties([#(dynamic.string("name"), dynamic.int(123))])
    |> decode.run(decoder)
  assert value == [DecodeError("String", "Int", ["name"])]
}

pub fn subfield_int_index_ok_test() {
  let decoder = {
    use x <- decode.subfield([0, 1], decode.string)
    use y <- decode.subfield([1, 0], decode.string)
    decode.success(#(x, y))
  }

  let assert Ok(value) =
    dynamic.array([
      dynamic.array(["one", "two", "three"] |> list.map(dynamic.string)),
      dynamic.array(["a", "b"] |> list.map(dynamic.string)),
    ])
    |> decode.run(decoder)
  assert value == #("two", "a")
}

pub fn subfield_wrong_inner_error_test() {
  let data = dynamic.properties([#(dynamic.string("name"), dynamic.int(123))])
  let assert Error(value) =
    decode.run(data, {
      use name <- decode.field("name", decode.string)
      decode.success(name)
    })
  assert value == [DecodeError("String", "Int", ["name"])]
}

pub fn optional_field_wrong_inner_error_test() {
  let data = dynamic.properties([#(dynamic.string("a"), dynamic.nil())])
  let assert Error(value) =
    decode.run(data, {
      use bar <- decode.optional_field("a", "", decode.string)
      decode.success(bar)
    })
  assert value == [DecodeError("String", "Nil", ["a"])]
}

pub fn sub_optional_field_wrong_inner_error_test() {
  let data =
    dynamic.properties([
      #(
        dynamic.string("a"),
        dynamic.properties([#(dynamic.string("b"), dynamic.nil())]),
      ),
    ])
  let assert Error(value) =
    decode.run(data, {
      use bar <- decode.optional_field("a", "", {
        use foo <- decode.optional_field("b", "", decode.string)
        decode.success(foo)
      })
      decode.success(bar)
    })
  assert value == [DecodeError("String", "Nil", ["a", "b"])]
}

pub fn optional_field_wrong_inner_error_type_test() {
  let data = dynamic.properties([#(dynamic.string("a"), dynamic.int(0))])
  let assert Error(value) =
    decode.run(data, {
      use bar <- decode.optional_field("a", "", decode.string)
      decode.success(bar)
    })
  assert value == [DecodeError("String", "Int", ["a"])]
}

pub fn string_map_ok_test() {
  let assert Ok(value) =
    dynamic.string("tEsT")
    |> decode.run(decode.string |> decode.map(string.lowercase))
  assert value == "test"
}

pub fn string_map_error_test() {
  let assert Error(_) =
    dynamic.int(0)
    |> decode.run(decode.string |> decode.map(string.lowercase))
}

pub fn string_ok_test() {
  let assert Ok(value) =
    dynamic.string("Hello!")
    |> decode.run(decode.string)
  assert value == "Hello!"
}

pub fn string_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.string)
  assert value == [DecodeError("String", "Int", [])]
}

pub fn dynamic_test() {
  let data = dynamic.int(123)
  let assert Ok(value) =
    data
    |> decode.run(decode.dynamic)
  assert value == data
}

pub fn int_ok_test() {
  let assert Ok(value) =
    dynamic.int(123)
    |> decode.run(decode.int)
  assert value == 123
}

pub fn int_error_test() {
  let assert Error(value) =
    dynamic.string("123")
    |> decode.run(decode.int)
  assert value == [DecodeError("Int", "String", [])]
}

pub fn float_ok_test() {
  let assert Ok(value) =
    dynamic.float(123.45)
    |> decode.run(decode.float)
  assert value == 123.45
}

pub fn float_error_test() {
  let assert Error(value) =
    dynamic.string("123.45")
    |> decode.run(decode.float)
  assert value == [DecodeError("Float", "String", [])]
}

pub fn bool_true_test() {
  let assert Ok(value) =
    dynamic.bool(True)
    |> decode.run(decode.bool)
  assert value == True
}

pub fn bool_false_test() {
  let assert Ok(value) =
    dynamic.bool(False)
    |> decode.run(decode.bool)
  assert value == False
}

pub fn bool_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.bool)
  assert value == [DecodeError("Bool", "Int", [])]
}

pub fn bit_array_ok_test() {
  let assert Ok(value) =
    dynamic.bit_array(<<1, 5, 3>>)
    |> decode.run(decode.bit_array)
  assert value == <<1, 5, 3>>
}

pub fn bit_array_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.bit_array)
  assert value == [DecodeError("BitArray", "Int", [])]
}

pub fn list_tuple_ok_test() {
  let assert Ok(value) =
    dynamic.array([dynamic.string("Hello"), dynamic.string("Joe")])
    |> decode.run(decode.list(decode.string))
  assert value == ["Hello", "Joe"]
}

pub fn list_string_ok_test() {
  let assert Ok(value) =
    dynamic.list(["Hello", "Joe"] |> list.map(dynamic.string))
    |> decode.run(decode.list(decode.string))
  assert value == ["Hello", "Joe"]
}

pub fn list_bool_ok_test() {
  let assert Ok(value) =
    dynamic.list([True, False] |> list.map(dynamic.bool))
    |> decode.run(decode.list(decode.bool))
  assert value == [True, False]
}

pub fn list_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.list(decode.int))
  assert value == [DecodeError("List", "Int", [])]
}

pub fn list_inner_0_error_test() {
  let assert Error(value) =
    dynamic.list([dynamic.int(1), dynamic.int(2)])
    |> decode.run(decode.list(decode.string))
  assert value == [DecodeError("String", "Int", ["0"])]
}

pub fn list_inner_1_error_test() {
  let assert Error(value) =
    dynamic.list([dynamic.string("1"), dynamic.int(2)])
    |> decode.run(decode.list(decode.string))
  assert value == [DecodeError("String", "Int", ["1"])]
}

pub fn list_tuple_inner_1_error_test() {
  let assert Error(value) =
    dynamic.array([dynamic.string("1"), dynamic.int(2)])
    |> decode.run(decode.list(decode.string))
  assert value == [DecodeError("String", "Int", ["1"])]
}

pub fn dict_ok_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("first"), dynamic.int(1)),
      #(dynamic.string("second"), dynamic.int(2)),
    ])
    |> decode.run(decode.dict(decode.string, decode.int))
  assert value == dict.from_list([#("first", 1), #("second", 2)])
}

pub fn dict_value_error_test() {
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.float(1.1), dynamic.int(1)),
      #(dynamic.float(1.2), dynamic.int(2)),
    ])
    |> decode.run(decode.dict(decode.float, decode.string))
  assert value == [DecodeError("String", "Int", ["values"])]
}

pub fn dict_key_error_test() {
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.float(1.1), dynamic.int(1)),
      #(dynamic.float(1.2), dynamic.int(2)),
    ])
    |> decode.run(decode.dict(decode.string, decode.int))
  assert value == [DecodeError("String", "Float", ["keys"])]
}

pub fn dict_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.dict(decode.string, decode.int))
  assert value == [DecodeError("Dict", "Int", [])]
}

pub fn at_dict_string_ok_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([
          #(
            dynamic.string("second"),
            dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.at(["first", "second", "third"], decode.int))
  assert value == 1337
}

pub fn at_dict_int_ok_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(
        dynamic.int(10),
        dynamic.properties([
          #(
            dynamic.int(20),
            dynamic.properties([#(dynamic.int(30), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.at([10, 20, 30], decode.int))
  assert value == 1337
}

pub fn at_tuple_int_ok_test() {
  let assert Ok(value) =
    dynamic.array([
      dynamic.string("x"),
      dynamic.array(["a", "b", "c"] |> list.map(dynamic.string)),
      dynamic.string("z"),
    ])
    |> decode.run(decode.at([1, 0], decode.string))
  assert value == "a"
}

pub fn at_wrong_inner_error_test() {
  let assert Error(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([
          #(
            dynamic.string("second"),
            dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.at(["first", "second", "third"], decode.string))
  assert value == [DecodeError("String", "Int", ["first", "second", "third"])]
}

pub fn at_no_path_error_test() {
  let assert Error(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
      ),
    ])
    |> decode.run(decode.at(["first", "second", "third"], decode.int))
  assert value == [DecodeError("Field", "Nothing", ["first", "second"])]
}

pub fn optional_string_present_ok_test() {
  let assert Ok(value) =
    dynamic.string("Hello, Joe!")
    |> decode.run(decode.optional(decode.string))
  assert value == option.Some("Hello, Joe!")
}

pub fn optional_bool_present_ok_test() {
  let assert Ok(value) =
    dynamic.bool(True)
    |> decode.run(decode.optional(decode.bool))
  assert value == option.Some(True)
}

pub fn optional_bool_absent_nil_ok_test() {
  let assert Ok(value) =
    dynamic.nil()
    |> decode.run(decode.optional(decode.bool))
  assert value == option.None
}

pub fn optional_error_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.optional(decode.string))
  assert value == [DecodeError("String", "Int", [])]
}

pub fn map_test() {
  let assert Ok(value) =
    dynamic.int(123)
    |> decode.run(decode.int |> decode.map(int.to_string))
  assert value == "123"
}

pub fn map_errors_test() {
  let decoder =
    decode.at(
      ["data"],
      decode.map_errors(decode.string, fn(errors) {
        let assert [DecodeError("String", "Int", [])] = errors
        [
          DecodeError("Wibble", "Wobble", ["ok"]),
          DecodeError("Wabble", "Wubble", ["ok"]),
        ]
      }),
    )

  let assert Error(value) =
    dynamic.properties([#(dynamic.string("data"), dynamic.int(123))])
    |> decode.run(decoder)
  assert value
    == [
      DecodeError("Wibble", "Wobble", ["data", "ok"]),
      DecodeError("Wabble", "Wubble", ["data", "ok"]),
    ]
}

pub fn collapse_errors_test() {
  let assert Error(value) =
    dynamic.properties([#(dynamic.string("data"), dynamic.int(123))])
    |> decode.run(decode.at(
      ["data"],
      decode.string |> decode.collapse_errors("Wibble"),
    ))
  assert value == [DecodeError("Wibble", "Int", ["data"])]
}

pub fn then_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("key"), dynamic.int(1)),
      #(dynamic.string("value"), dynamic.int(100)),
    ])
    |> decode.run(decoder)
  assert value == AnInt(100)

  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("key"), dynamic.int(2)),
      #(dynamic.string("value"), dynamic.string("Hi!")),
    ])
    |> decode.run(decoder)
  assert value == AString("Hi!")
}

type IntOrString {
  AnInt(Int)
  AString(String)
}

pub fn then_error_0_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decoder)
  assert value == [DecodeError("Dict", "Int", [])]
}

pub fn then_error_1_test() {
  let decoder =
    decode.at(["key"], decode.int)
    |> decode.then(fn(i) {
      decode.at(["value"], case i {
        1 -> decode.int |> decode.map(AnInt)
        _ -> decode.string |> decode.map(AString)
      })
    })

  let assert Error(value) =
    dynamic.properties([
      #(dynamic.string("key"), dynamic.int(1)),
      #(dynamic.string("value"), dynamic.string("Hi!")),
    ])
    |> decode.run(decoder)
  assert value == [DecodeError("Int", "String", ["value"])]
}

pub type MyEnum {
  A
  B
  C
}

pub fn then_enum_test() {
  let decoder =
    decode.string
    |> decode.then(fn(s) {
      case s {
        "a" -> decode.success(A)
        "b" -> decode.success(B)
        "c" -> decode.success(C)
        _ -> decode.failure(A, "MyEnum")
      }
    })

  let assert Ok(value) = decode.run(dynamic.string("a"), decoder)
  assert value == A

  let assert Ok(value) = decode.run(dynamic.string("b"), decoder)
  assert value == B

  let assert Ok(value) = decode.run(dynamic.string("c"), decoder)
  assert value == C

  let assert Error(value) = decode.run(dynamic.string("d"), decoder)
  assert value == [DecodeError("MyEnum", "String", [])]
}

pub fn one_of_ok_0_test() {
  let assert Ok(value) =
    dynamic.string("Hello!")
    |> decode.run(
      decode.one_of(decode.string, [decode.int |> decode.map(int.to_string)]),
    )
  assert value == "Hello!"
}

pub fn one_of_ok_1_test() {
  let decoder =
    decode.one_of(decode.string, [
      decode.int
      |> decode.map(int.to_string),
    ])
  let assert Ok(value) =
    dynamic.int(123)
    |> decode.run(decoder)
  assert value == "123"
}

pub fn one_of_ok_2_test() {
  let decoder =
    decode.one_of(decode.string, [
      decode.int |> decode.map(int.to_string),
      decode.float |> decode.map(float.to_string),
    ])
  let assert Ok(value) =
    dynamic.float(12.45)
    |> decode.run(decoder)
  assert value == "12.45"
}

pub fn one_of_error_test() {
  let decoder =
    decode.one_of(decode.string, or: [
      decode.int
      |> decode.map(int.to_string),
    ])
  let assert Error(value) =
    dynamic.float(1.2)
    |> decode.run(decoder)
  assert value == [DecodeError("String", "Float", [])]
}

pub fn failure_test() {
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decode.failure(1, "WibbleWobble"))
  assert value == [DecodeError("WibbleWobble", "Int", [])]
}

pub fn variants_test() {
  let decoder = {
    use tag <- decode.field("tag", decode.string)
    case tag {
      "int" -> {
        use int <- decode.field("the-int", decode.int)
        decode.success(AnInt(int))
      }
      "string" -> {
        use string <- decode.field("the-string", decode.string)
        decode.success(AString(string))
      }
      _ -> {
        decode.failure(AnInt(0), "IntOrString")
      }
    }
  }

  // Int variant
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("tag"), dynamic.string("int")),
      #(dynamic.string("the-int"), dynamic.int(123)),
    ])
    |> decode.run(decoder)
  assert value == AnInt(123)

  // String variant
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("tag"), dynamic.string("string")),
      #(dynamic.string("the-string"), dynamic.string("hello")),
    ])
    |> decode.run(decoder)
  assert value == AString("hello")

  // Invalid tag
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.string("tag"), dynamic.string("dunno")),
      #(dynamic.string("the-string"), dynamic.string("hello")),
    ])
    |> decode.run(decoder)
  assert value == [DecodeError("IntOrString", "Dict", [])]

  // Missing tag
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.string("the-string"), dynamic.string("hello")),
    ])
    |> decode.run(decoder)
  assert value
    == [
      DecodeError("Field", "Nothing", ["tag"]),
      DecodeError("IntOrString", "Dict", []),
    ]

  // String invalid field
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.string("tag"), dynamic.string("string")),
      #(dynamic.string("the-string"), dynamic.float(12.3)),
    ])
    |> decode.run(decoder)
  assert value == [DecodeError("String", "Float", ["the-string"])]
}

pub type PocketMonsterType {
  Fire
  Water
  Grass
  Electric
}

pub fn documentation_enum_example_test() {
  let decoder = {
    use decoded_string <- decode.then(decode.string)
    case decoded_string {
      // Return succeeding decoders for valid strings
      "fire" -> decode.success(Fire)
      "water" -> decode.success(Water)
      "grass" -> decode.success(Grass)
      "electric" -> decode.success(Electric)
      // Return a failing decoder for any other strings
      _ -> decode.failure(Fire, "PocketMonsterType")
    }
  }

  let assert Ok(value) = decode.run(dynamic.string("water"), decoder)
  assert value == Water

  let assert Error(value) = decode.run(dynamic.string("wobble"), decoder)
  assert value == [DecodeError("PocketMonsterType", "String", [])]
}

pub type PocketMonsterPerson {
  Trainer(name: String, badge_count: Int)
  GymLeader(name: String, speciality: String)
}

pub fn documentation_variants_example_test() {
  let trainer_decoder = {
    use name <- decode.field("name", decode.string)
    use badge_count <- decode.field("badge-count", decode.int)
    decode.success(Trainer(name, badge_count))
  }

  let gym_leader_decoder = {
    use name <- decode.field("name", decode.string)
    use speciality <- decode.field("speciality", decode.string)
    decode.success(GymLeader(name, speciality))
  }

  let decoder = {
    use tag <- decode.field("type", decode.string)
    case tag {
      "gym-leader" -> gym_leader_decoder
      _ -> trainer_decoder
    }
  }

  // Trainer
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("type"), dynamic.string("trainer")),
      #(dynamic.string("name"), dynamic.string("Ash")),
      #(dynamic.string("badge-count"), dynamic.int(8)),
    ])
    |> decode.run(decoder)
  assert value == Trainer("Ash", 8)

  // Gym leader
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("type"), dynamic.string("gym-leader")),
      #(dynamic.string("name"), dynamic.string("Brock")),
      #(dynamic.string("speciality"), dynamic.string("Rock")),
    ])
    |> decode.run(decoder)
  assert value == GymLeader("Brock", "Rock")

  // Error
  let assert Error(value) =
    dynamic.properties([
      #(dynamic.string("type"), dynamic.string("gym-leader")),
      #(dynamic.string("name"), dynamic.string("Brock")),
    ])
    |> decode.run(decoder)
  assert value
    == [DecodeError(expected: "Field", found: "Nothing", path: ["speciality"])]
}

fn decode_string(data: Dynamic) -> Result(String, String) {
  decode.run(data, decode.string)
  |> result.replace_error("")
}

fn decode_float(data: Dynamic) -> Result(Float, Float) {
  decode.run(data, decode.float)
  |> result.replace_error(0.0)
}

pub fn new_primitive_decoder_string_ok_test() {
  let decoder = decode.new_primitive_decoder("String", decode_string)
  let assert Ok(value) =
    dynamic.string("Hello!")
    |> decode.run(decoder)
  assert value == "Hello!"
}

pub fn new_primitive_decoder_string_error_test() {
  let decoder = decode.new_primitive_decoder("String", decode_string)
  let assert Error(value) =
    dynamic.int(123)
    |> decode.run(decoder)
  assert value == [DecodeError("String", "Int", [])]
}

pub fn new_primitive_decoder_float_ok_test() {
  let decoder = decode.new_primitive_decoder("Float", decode_float)
  let assert Ok(value) =
    dynamic.float(12.4)
    |> decode.run(decoder)
  assert value == 12.4
}

pub fn new_primitive_decoder_float_error_test() {
  let decoder = decode.new_primitive_decoder("Float", decode_float)
  let assert Error(value) =
    dynamic.string("blah")
    |> decode.run(decoder)
  assert value == [DecodeError("Float", "String", [])]
}

pub type LinkedList {
  ListEmpty
  ListNonEmpty(element: Int, tail: LinkedList)
}

pub fn list_decoder() -> decode.Decoder(LinkedList) {
  use tag <- decode.field("type", decode.string)
  case tag {
    "list-non-empty" -> {
      use element <- decode.field("element", decode.int)
      use tail <- decode.field("tail", list_decoder())
      decode.success(ListNonEmpty(element: element, tail: tail))
    }
    _ -> decode.success(ListEmpty)
  }
}

pub fn recursive_data_structure_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(dynamic.string("type"), dynamic.string("list-non-empty")),
      #(dynamic.string("element"), dynamic.int(1)),
      #(
        dynamic.string("tail"),
        dynamic.properties([
          #(dynamic.string("type"), dynamic.string("list-non-empty")),
          #(dynamic.string("element"), dynamic.int(2)),
          #(
            dynamic.string("tail"),
            dynamic.properties([
              #(dynamic.string("type"), dynamic.string("list-empty")),
            ]),
          ),
        ]),
      ),
    ])
    |> decode.run(list_decoder())
  assert value == ListNonEmpty(1, ListNonEmpty(2, ListEmpty))
}

pub fn optionally_at_dict_string_ok_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([
          #(
            dynamic.string("second"),
            dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.optionally_at(
      ["first", "second", "third"],
      100,
      decode.int,
    ))
  assert value == 1337
}

pub fn optionally_at_dict_int_ok_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(
        dynamic.int(10),
        dynamic.properties([
          #(
            dynamic.int(20),
            dynamic.properties([#(dynamic.int(30), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.optionally_at([10, 20, 30], 123, decode.int))
  assert value == 1337
}

pub fn optionally_at_tuple_int_ok_test() {
  let assert Ok(value) =
    dynamic.array([
      dynamic.string("x"),
      dynamic.array(["a", "b", "c"] |> list.map(dynamic.string)),
      dynamic.string("z"),
    ])
    |> decode.run(decode.optionally_at([1, 0], "something", decode.string))
  assert value == "a"
}

pub fn optionally_at_wrong_inner_error_test() {
  let assert Error(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([
          #(
            dynamic.string("second"),
            dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
          ),
        ]),
      ),
    ])
    |> decode.run(decode.optionally_at(
      ["first", "second", "third"],
      "default",
      decode.string,
    ))
  assert value == [DecodeError("String", "Int", ["first", "second", "third"])]
}

pub fn optionally_at_no_path_error_test() {
  let assert Ok(value) =
    dynamic.properties([
      #(
        dynamic.string("first"),
        dynamic.properties([#(dynamic.string("third"), dynamic.int(1337))]),
      ),
    ])
    |> decode.run(decode.optionally_at(
      ["first", "second", "third"],
      100,
      decode.int,
    ))
  assert value == 100
}

@external(erlang, "maps", "from_list")
@external(javascript, "../../gleam_stdlib_test_ffi.mjs", "object")
fn make_object(items: List(#(String, t))) -> Dynamic

@external(erlang, "maps", "from_list")
@external(javascript, "../../gleam_stdlib_test_ffi.mjs", "map")
fn make_map(items: List(#(String, t))) -> Dynamic

pub fn js_object_test() {
  let assert Ok(value) =
    [#("a", 10), #("b", 20), #("c", 30)]
    |> make_object
    |> decode.run(decode.dict(decode.string, decode.int))
  assert value == dict.from_list([#("a", 10), #("b", 20), #("c", 30)])
}

pub fn js_map_test() {
  let assert Ok(value) =
    [#("a", 10), #("b", 20), #("c", 30)]
    |> make_map
    |> decode.run(decode.dict(decode.string, decode.int))
  assert value == dict.from_list([#("a", 10), #("b", 20), #("c", 30)])
}

type Nested {
  Nested(List(Nested))
  Value(String)
}

fn recursive_decoder() -> decode.Decoder(Nested) {
  use <- decode.recursive()
  decode.one_of(decode.string |> decode.map(Value), [
    decode.list(recursive_decoder()) |> decode.map(Nested),
  ])
}

pub fn recursive_test() {
  let expected =
    Nested([
      Nested([Value("one"), Value("two")]),
      Nested([Value("three")]),
      Nested([]),
    ])

  let assert Ok(value) =
    dynamic.list(
      [
        ["one", "two"] |> list.map(dynamic.string),
        ["three"] |> list.map(dynamic.string),
        [],
      ]
      |> list.map(dynamic.list),
    )
    |> decode.run(recursive_decoder())
  assert value == expected
}

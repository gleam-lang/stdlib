import gleam/dict
import gleam/dynamic.{DecodeError}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/option
import gleam/should

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
    dynamic.from(
      dict.from_list([
        #("name", dynamic.from("Nubi")),
        #("email", dynamic.from("nubi@example.com")),
        #("is_admin", dynamic.from(False)),
        #("is_confirmed", dynamic.from(True)),
        #("score", dynamic.from(180)),
      ]),
    )

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

  decode.run(data, decoder)
  |> should.be_ok
  |> should.equal(User("Nubi", "nubi@example.com", False, True, 180))
}

pub fn field_ok_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from("Nubi"))]))
  let decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }

  decode.run(data, decoder)
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn subfield_ok_test() {
  let data =
    dynamic.from(
      dict.from_list([
        #("person", dict.from_list([#("name", dynamic.from("Nubi"))])),
      ]),
    )
  let decoder = {
    use name <- decode.subfield(["person", "name"], decode.string)
    decode.success(name)
  }

  decode.run(data, decoder)
  |> should.be_ok
  |> should.equal("Nubi")
}

pub fn field_int_index_ok_test() {
  let decoder = {
    use x <- decode.field(0, decode.string)
    use y <- decode.field(1, decode.string)
    decode.success(#(x, y))
  }

  dynamic.from(#("one", "two", "three"))
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(#("one", "two"))
}

pub fn field_int_index_list_ok_test() {
  let decoder = {
    use x <- decode.field(0, decode.string)
    use y <- decode.field(1, decode.string)
    decode.success(#(x, y))
  }

  dynamic.from(["one", "two", "three", "four"])
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(#("one", "two"))
}

pub fn subfield_not_found_error_test() {
  let decoder = {
    use name <- decode.subfield(["name"], decode.string)
    decode.success(name)
  }

  dynamic.from(123)
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_not_found_error_test() {
  let decoder = {
    use name <- decode.subfield(["name"], decode.string)
    decode.success(name)
  }

  dynamic.from(123)
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn field_wrong_inner_error_test() {
  let decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }

  dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn subfield_int_index_ok_test() {
  let decoder = {
    use x <- decode.subfield([0, 1], decode.string)
    use y <- decode.subfield([1, 0], decode.string)
    decode.success(#(x, y))
  }

  dynamic.from(#(#("one", "two", "three"), #("a", "b")))
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(#("two", "a"))
}

pub fn subfield_wrong_inner_error_test() {
  let data = dynamic.from(dict.from_list([#("name", dynamic.from(123))]))
  decode.run(data, {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  })
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["name"])])
}

pub fn string_ok_test() {
  dynamic.from("Hello!")
  |> decode.run(decode.string)
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn string_error_test() {
  dynamic.from(123)
  |> decode.run(decode.string)
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn dynamic_test() {
  let data = dynamic.from(123)
  data
  |> decode.run(decode.dynamic)
  |> should.be_ok
  |> should.equal(data)
}

pub fn int_ok_test() {
  dynamic.from(123)
  |> decode.run(decode.int)
  |> should.be_ok
  |> should.equal(123)
}

pub fn int_error_test() {
  dynamic.from("123")
  |> decode.run(decode.int)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", [])])
}

pub fn float_ok_test() {
  dynamic.from(123.45)
  |> decode.run(decode.float)
  |> should.be_ok
  |> should.equal(123.45)
}

pub fn float_error_test() {
  dynamic.from("123.45")
  |> decode.run(decode.float)
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
}

pub fn bool_true_test() {
  dynamic.from(True)
  |> decode.run(decode.bool)
  |> should.be_ok
  |> should.equal(True)
}

pub fn bool_false_test() {
  dynamic.from(False)
  |> decode.run(decode.bool)
  |> should.be_ok
  |> should.equal(False)
}

pub fn bool_error_test() {
  dynamic.from(123)
  |> decode.run(decode.bool)
  |> should.be_error
  |> should.equal([DecodeError("Bool", "Int", [])])
}

pub fn bit_array_ok_test() {
  dynamic.from(<<1, 5, 3>>)
  |> decode.run(decode.bit_array)
  |> should.be_ok
  |> should.equal(<<1, 5, 3>>)
}

pub fn bit_array_error_test() {
  dynamic.from(123)
  |> decode.run(decode.bit_array)
  |> should.be_error
  |> should.equal([DecodeError("BitArray", "Int", [])])
}

pub fn list_tuple_ok_test() {
  dynamic.from(#("Hello", "Joe"))
  |> decode.run(decode.list(decode.string))
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_string_ok_test() {
  dynamic.from(["Hello", "Joe"])
  |> decode.run(decode.list(decode.string))
  |> should.be_ok
  |> should.equal(["Hello", "Joe"])
}

pub fn list_bool_ok_test() {
  dynamic.from([True, False])
  |> decode.run(decode.list(decode.bool))
  |> should.be_ok
  |> should.equal([True, False])
}

pub fn list_error_test() {
  dynamic.from(123)
  |> decode.run(decode.list(decode.int))
  |> should.be_error
  |> should.equal([DecodeError("List", "Int", [])])
}

pub fn list_inner_0_error_test() {
  dynamic.from([1, 2])
  |> decode.run(decode.list(decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["0"])])
}

pub fn list_inner_1_error_test() {
  dynamic.from([dynamic.from("1"), dynamic.from(2)])
  |> decode.run(decode.list(decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["1"])])
}

pub fn list_tuple_inner_1_error_test() {
  dynamic.from(#("1", 2))
  |> decode.run(decode.list(decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["1"])])
}

pub fn dict_ok_test() {
  let values = dict.from_list([#("first", 1), #("second", 2)])
  dynamic.from(values)
  |> decode.run(decode.dict(decode.string, decode.int))
  |> should.be_ok
  |> should.equal(values)
}

pub fn dict_value_error_test() {
  dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  |> decode.run(decode.dict(decode.float, decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["values"])])
}

pub fn dict_key_error_test() {
  dynamic.from(dict.from_list([#(1.1, 1), #(1.2, 2)]))
  |> decode.run(decode.dict(decode.string, decode.int))
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["keys"])])
}

pub fn dict_error_test() {
  dynamic.from(123)
  |> decode.run(decode.dict(decode.string, decode.int))
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
}

pub fn at_dict_string_ok_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> decode.run(decode.at(["first", "second", "third"], decode.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_dict_int_ok_test() {
  dynamic.from(
    dict.from_list([
      #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
    ]),
  )
  |> decode.run(decode.at([10, 20, 30], decode.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn at_tuple_int_ok_test() {
  dynamic.from(#("x", #("a", "b", "c"), "z"))
  |> decode.run(decode.at([1, 0], decode.string))
  |> should.be_ok
  |> should.equal("a")
}

pub fn at_wrong_inner_error_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> decode.run(decode.at(["first", "second", "third"], decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
}

pub fn at_no_path_error_test() {
  dynamic.from(dict.from_list([#("first", dict.from_list([#("third", 1337)]))]))
  |> decode.run(decode.at(["first", "second", "third"], decode.int))
  |> should.be_error
  |> should.equal([DecodeError("Field", "Nothing", ["first", "second"])])
}

pub fn optional_string_present_ok_test() {
  dynamic.from("Hello, Joe!")
  |> decode.run(decode.optional(decode.string))
  |> should.be_ok
  |> should.equal(option.Some("Hello, Joe!"))
}

pub fn optional_bool_present_ok_test() {
  dynamic.from(True)
  |> decode.run(decode.optional(decode.bool))
  |> should.be_ok
  |> should.equal(option.Some(True))
}

pub fn optional_bool_absent_nil_ok_test() {
  dynamic.from(Nil)
  |> decode.run(decode.optional(decode.bool))
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_bool_absent_none_ok_test() {
  dynamic.from(option.None)
  |> decode.run(decode.optional(decode.bool))
  |> should.be_ok
  |> should.equal(option.None)
}

pub fn optional_error_test() {
  dynamic.from(123)
  |> decode.run(decode.optional(decode.string))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn map_test() {
  dynamic.from(123)
  |> decode.run(decode.int |> decode.map(int.to_string))
  |> should.be_ok
  |> should.equal("123")
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

  dynamic.from(dict.from_list([#("data", 123)]))
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError("Wibble", "Wobble", ["data", "ok"]),
    DecodeError("Wabble", "Wubble", ["data", "ok"]),
  ])
}

pub fn collapse_errors_test() {
  dynamic.from(dict.from_list([#("data", 123)]))
  |> decode.run(decode.at(
    ["data"],
    decode.string |> decode.collapse_errors("Wibble"),
  ))
  |> should.be_error
  |> should.equal([DecodeError("Wibble", "Int", ["data"])])
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

  dynamic.from(dict.from_list([#("key", 1), #("value", 100)]))
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(AnInt(100))

  dynamic.from(
    dict.from_list([#("key", dynamic.from(2)), #("value", dynamic.from("Hi!"))]),
  )
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(AString("Hi!"))
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

  dynamic.from(123)
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Dict", "Int", [])])
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

  dynamic.from(
    dict.from_list([#("key", dynamic.from(1)), #("value", dynamic.from("Hi!"))]),
  )
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("Int", "String", ["value"])])
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

  decode.run(dynamic.from("a"), decoder)
  |> should.be_ok
  |> should.equal(A)

  decode.run(dynamic.from("b"), decoder)
  |> should.be_ok
  |> should.equal(B)

  decode.run(dynamic.from("c"), decoder)
  |> should.be_ok
  |> should.equal(C)

  decode.run(dynamic.from("d"), decoder)
  |> should.be_error
  |> should.equal([DecodeError("MyEnum", "String", [])])
}

pub fn one_of_ok_0_test() {
  dynamic.from("Hello!")
  |> decode.run(
    decode.one_of(decode.string, [decode.int |> decode.map(int.to_string)]),
  )
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn one_of_ok_1_test() {
  let decoder =
    decode.one_of(decode.string, [
      decode.int
      |> decode.map(int.to_string),
    ])
  dynamic.from(123)
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal("123")
}

pub fn one_of_ok_2_test() {
  let decoder =
    decode.one_of(decode.string, [
      decode.int |> decode.map(int.to_string),
      decode.float |> decode.map(float.to_string),
    ])
  dynamic.from(12.45)
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal("12.45")
}

pub fn one_of_error_test() {
  let decoder =
    decode.one_of(decode.string, or: [
      decode.int
      |> decode.map(int.to_string),
    ])
  dynamic.from(1.2)
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", [])])
}

pub fn failure_test() {
  dynamic.from(123)
  |> decode.run(decode.failure(1, "WibbleWobble"))
  |> should.be_error
  |> should.equal([DecodeError("WibbleWobble", "Int", [])])
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
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("int")),
      #("the-int", dynamic.from(123)),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(AnInt(123))

  // String variant
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("string")),
      #("the-string", dynamic.from("hello")),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(AString("hello"))

  // Invalid tag
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("dunno")),
      #("the-string", dynamic.from("hello")),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("IntOrString", "Dict", [])])

  // Missing tag
  dynamic.from(dict.from_list([#("the-string", dynamic.from("hello"))]))
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError("Field", "Nothing", ["tag"]),
    DecodeError("IntOrString", "Dict", []),
  ])

  // String invalid field
  dynamic.from(
    dict.from_list([
      #("tag", dynamic.from("string")),
      #("the-string", dynamic.from(12.3)),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([DecodeError("String", "Float", ["the-string"])])
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

  decode.run(dynamic.from("water"), decoder)
  |> should.be_ok
  |> should.equal(Water)

  decode.run(dynamic.from("wobble"), decoder)
  |> should.be_error
  |> should.equal([DecodeError("PocketMonsterType", "String", [])])
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
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("trainer")),
      #("name", dynamic.from("Ash")),
      #("badge-count", dynamic.from(8)),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(Trainer("Ash", 8))

  // Gym leader
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("gym-leader")),
      #("name", dynamic.from("Brock")),
      #("speciality", dynamic.from("Rock")),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_ok
  |> should.equal(GymLeader("Brock", "Rock"))

  // Error
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("gym-leader")),
      #("name", dynamic.from("Brock")),
    ]),
  )
  |> decode.run(decoder)
  |> should.be_error
  |> should.equal([
    DecodeError(expected: "Field", found: "Nothing", path: ["speciality"]),
  ])
}

pub fn new_primitive_decoder_string_ok_test() {
  dynamic.from("Hello!")
  |> decode.run(decode.new_primitive_decoder(dynamic.string, ""))
  |> should.be_ok
  |> should.equal("Hello!")
}

pub fn new_primitive_decoder_string_error_test() {
  dynamic.from(123)
  |> decode.run(decode.new_primitive_decoder(dynamic.string, ""))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", [])])
}

pub fn new_primitive_decoder_float_ok_test() {
  dynamic.from(12.4)
  |> decode.run(decode.new_primitive_decoder(dynamic.float, 0.0))
  |> should.be_ok
  |> should.equal(12.4)
}

pub fn new_primitive_decoder_float_error_test() {
  dynamic.from("blah")
  |> decode.run(decode.new_primitive_decoder(dynamic.float, 0.0))
  |> should.be_error
  |> should.equal([DecodeError("Float", "String", [])])
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
  dynamic.from(
    dict.from_list([
      #("type", dynamic.from("list-non-empty")),
      #("element", dynamic.from(1)),
      #(
        "tail",
        dynamic.from(
          dict.from_list([
            #("type", dynamic.from("list-non-empty")),
            #("element", dynamic.from(2)),
            #(
              "tail",
              dynamic.from(
                dict.from_list([#("type", dynamic.from("list-empty"))]),
              ),
            ),
          ]),
        ),
      ),
    ]),
  )
  |> decode.run(list_decoder())
  |> should.be_ok
  |> should.equal(ListNonEmpty(1, ListNonEmpty(2, ListEmpty)))
}

pub fn optionally_at_dict_string_ok_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> decode.run(decode.optionally_at(
    ["first", "second", "third"],
    100,
    decode.int,
  ))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn optionally_at_dict_int_ok_test() {
  dynamic.from(
    dict.from_list([
      #(10, dict.from_list([#(20, dict.from_list([#(30, 1337)]))])),
    ]),
  )
  |> decode.run(decode.optionally_at([10, 20, 30], 123, decode.int))
  |> should.be_ok
  |> should.equal(1337)
}

pub fn optionally_at_tuple_int_ok_test() {
  dynamic.from(#("x", #("a", "b", "c"), "z"))
  |> decode.run(decode.optionally_at([1, 0], "something", decode.string))
  |> should.be_ok
  |> should.equal("a")
}

pub fn optionally_at_wrong_inner_error_test() {
  dynamic.from(
    dict.from_list([
      #(
        "first",
        dict.from_list([#("second", dict.from_list([#("third", 1337)]))]),
      ),
    ]),
  )
  |> decode.run(decode.optionally_at(
    ["first", "second", "third"],
    "default",
    decode.string,
  ))
  |> should.be_error
  |> should.equal([DecodeError("String", "Int", ["first", "second", "third"])])
}

pub fn optionally_at_no_path_error_test() {
  dynamic.from(dict.from_list([#("first", dict.from_list([#("third", 1337)]))]))
  |> decode.run(decode.optionally_at(
    ["first", "second", "third"],
    100,
    decode.int,
  ))
  |> should.be_ok
  |> should.equal(100)
}

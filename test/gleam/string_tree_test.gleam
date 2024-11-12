import gleam/should
import gleam/string_tree

pub fn string_tree_test() {
  let data =
    string_tree.from_string("ello")
    |> string_tree.append(",")
    |> string_tree.append(" world!")
    |> string_tree.prepend("H")

  data
  |> string_tree.to_string
  |> should.equal("Hello, world!")

  data
  |> string_tree.byte_size
  |> should.equal(13)

  let data =
    string_tree.from_string("ello")
    |> string_tree.append_tree(string_tree.from_string(","))
    |> string_tree.append_tree(
      string_tree.concat([
        string_tree.from_string(" wo"),
        string_tree.from_string("rld!"),
      ]),
    )
    |> string_tree.prepend_tree(string_tree.from_string("H"))

  data
  |> string_tree.to_string
  |> should.equal("Hello, world!")

  data
  |> string_tree.byte_size
  |> should.equal(13)
}

pub fn reverse_test() {
  "Ĺo͂řȩm̅"
  |> string_tree.from_string
  |> string_tree.reverse
  |> string_tree.reverse
  |> string_tree.to_string
  |> should.equal("Ĺo͂řȩm̅")

  "Ĺo͂řȩm̅"
  |> string_tree.from_string
  |> string_tree.reverse
  |> string_tree.to_string
  |> should.equal("m̅ȩřo͂Ĺ")

  "👶🏿"
  |> string_tree.from_string
  |> string_tree.reverse
  |> string_tree.reverse
  |> string_tree.to_string
  |> should.equal("👶🏿")

  "👶🏿"
  |> string_tree.from_string
  |> string_tree.reverse
  |> string_tree.to_string
  |> should.equal("👶🏿")
}

pub fn lowercase_test() {
  ["Gleam", "Gleam"]
  |> string_tree.from_strings
  |> string_tree.lowercase
  |> string_tree.to_string
  |> should.equal("gleamgleam")
}

pub fn uppercase_test() {
  ["Gleam", "Gleam"]
  |> string_tree.from_strings
  |> string_tree.uppercase
  |> string_tree.to_string
  |> should.equal("GLEAMGLEAM")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string_tree.from_string
  |> string_tree.split(",")
  |> should.equal([
    string_tree.from_string("Gleam"),
    string_tree.from_string("Erlang"),
    string_tree.from_string("Elixir"),
  ])

  ["Gleam, Erl", "ang,Elixir"]
  |> string_tree.from_strings
  |> string_tree.split(", ")
  |> should.equal([
    string_tree.from_string("Gleam"),
    string_tree.from_strings(["Erl", "ang,Elixir"]),
  ])
}

pub fn is_equal_test() {
  string_tree.from_string("12")
  |> string_tree.is_equal(string_tree.from_strings(["1", "2"]))
  |> should.be_true

  string_tree.from_string("12")
  |> string_tree.is_equal(string_tree.from_string("12"))
  |> should.be_true

  string_tree.from_string("12")
  |> string_tree.is_equal(string_tree.from_string("2"))
  |> should.be_false
}

pub fn is_empty_test() {
  string_tree.from_string("")
  |> string_tree.is_empty
  |> should.be_true

  string_tree.from_string("12")
  |> string_tree.is_empty
  |> should.be_false

  string_tree.from_strings([])
  |> string_tree.is_empty
  |> should.be_true

  string_tree.from_strings(["", ""])
  |> string_tree.is_empty
  |> should.be_true
}

pub fn new_test() {
  string_tree.new()
  |> string_tree.to_string
  |> should.equal("")
}

pub fn join_test() {
  [
    string_tree.from_string("Gleam"),
    string_tree.from_string("Elixir"),
    string_tree.from_string("Erlang"),
  ]
  |> string_tree.join(", ")
  |> string_tree.to_string
  |> should.equal("Gleam, Elixir, Erlang")
}

import gleam/string_tree

pub fn string_tree_test() {
  let data =
    string_tree.from_string("ello")
    |> string_tree.append(",")
    |> string_tree.append(" world!")
    |> string_tree.prepend("H")

  assert data
    |> string_tree.to_string
    == "Hello, world!"

  assert data
    |> string_tree.byte_size
    == 13

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

  assert data
    |> string_tree.to_string
    == "Hello, world!"

  assert data
    |> string_tree.byte_size
    == 13
}

pub fn reverse_test() {
  assert "Ĺo͂řȩm̅"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.reverse
    |> string_tree.to_string
    == "Ĺo͂řȩm̅"

  assert "Ĺo͂řȩm̅"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.to_string
    == "m̅ȩřo͂Ĺ"

  assert "👶🏿"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.reverse
    |> string_tree.to_string
    == "👶🏿"

  assert "👶🏿"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.to_string
    == "👶🏿"
}

pub fn lowercase_test() {
  assert ["Gleam", "Gleam"]
    |> string_tree.from_strings
    |> string_tree.lowercase
    |> string_tree.to_string
    == "gleamgleam"
}

pub fn uppercase_test() {
  assert ["Gleam", "Gleam"]
    |> string_tree.from_strings
    |> string_tree.uppercase
    |> string_tree.to_string
    == "GLEAMGLEAM"
}

pub fn split_test() {
  assert "Gleam,Erlang,Elixir"
    |> string_tree.from_string
    |> string_tree.split(",")
    == [
      string_tree.from_string("Gleam"),
      string_tree.from_string("Erlang"),
      string_tree.from_string("Elixir"),
    ]

  assert ["Gleam, Erl", "ang,Elixir"]
    |> string_tree.from_strings
    |> string_tree.split(", ")
    == [
      string_tree.from_string("Gleam"),
      string_tree.from_strings(["Erl", "ang,Elixir"]),
    ]
}

pub fn is_equal_test() {
  assert string_tree.from_string("12")
    |> string_tree.is_equal(string_tree.from_strings(["1", "2"]))

  assert string_tree.from_string("12")
    |> string_tree.is_equal(string_tree.from_string("12"))

  assert !{
    string_tree.from_string("12")
    |> string_tree.is_equal(string_tree.from_string("2"))
  }
}

pub fn is_empty_test() {
  assert string_tree.from_string("")
    |> string_tree.is_empty

  assert !{
    string_tree.from_string("12")
    |> string_tree.is_empty
  }

  assert string_tree.from_strings([])
    |> string_tree.is_empty

  assert string_tree.from_strings(["", ""])
    |> string_tree.is_empty
}

pub fn new_test() {
  assert string_tree.new()
    |> string_tree.to_string
    == ""
}

pub fn join_test() {
  assert [
      string_tree.from_string("Gleam"),
      string_tree.from_string("Elixir"),
      string_tree.from_string("Erlang"),
    ]
    |> string_tree.join(", ")
    |> string_tree.to_string
    == "Gleam, Elixir, Erlang"
}

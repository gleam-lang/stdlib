import gleam/string_tree

pub fn string_tree_to_string_test() {
  let data =
    string_tree.from_string("ello")
    |> string_tree.append(",")
    |> string_tree.append(" world!")
    |> string_tree.prepend("H")

  assert string_tree.to_string(data) == "Hello, world!"
}

pub fn string_tree_byte_size_test() {
  let data =
    string_tree.from_string("ello")
    |> string_tree.append(",")
    |> string_tree.append(" world!")
    |> string_tree.prepend("H")

  assert string_tree.byte_size(data) == 13
}

pub fn string_tree_with_trees_to_string_test() {
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

  assert string_tree.to_string(data) == "Hello, world!"
}

pub fn string_tree_with_trees_byte_size_test() {
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

  assert string_tree.byte_size(data) == 13
}

pub fn reverse_roundtrip_test() {
  assert "Ĺo͂řȩm̅"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.reverse
    |> string_tree.to_string
    == "Ĺo͂řȩm̅"
}

pub fn reverse_test() {
  assert "Ĺo͂řȩm̅"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.to_string
    == "m̅ȩřo͂Ĺ"
}

pub fn reverse_emoji_roundtrip_test() {
  assert "👶🏿"
    |> string_tree.from_string
    |> string_tree.reverse
    |> string_tree.reverse
    |> string_tree.to_string
    == "👶🏿"
}

pub fn reverse_emoji_test() {
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

pub fn split_simple_test() {
  assert "Gleam,Erlang,Elixir"
    |> string_tree.from_string
    |> string_tree.split(",")
    == [
      string_tree.from_string("Gleam"),
      string_tree.from_string("Erlang"),
      string_tree.from_string("Elixir"),
    ]
}

pub fn split_multi_string_test() {
  assert ["Gleam, Erl", "ang,Elixir"]
    |> string_tree.from_strings
    |> string_tree.split(", ")
    == [
      string_tree.from_string("Gleam"),
      string_tree.from_strings(["Erl", "ang,Elixir"]),
    ]
}

// An empty pattern splits between every grapheme.
pub fn split_empty_pattern_test() {
  assert "abc"
    |> string_tree.from_string
    |> string_tree.split("")
    == [
      string_tree.from_string("a"),
      string_tree.from_string("b"),
      string_tree.from_string("c"),
    ]
}

// An empty pattern substitutes around every grapheme.
pub fn replace_empty_pattern_test() {
  assert "ab"
    |> string_tree.from_string
    |> string_tree.replace(each: "", with: "-")
    |> string_tree.to_string
    == "-a-b-"
}

pub fn replace_empty_pattern_empty_string_test() {
  assert ""
    |> string_tree.from_string
    |> string_tree.replace(each: "", with: "-")
    |> string_tree.to_string
    == "-"
}

pub fn is_equal_different_structure_test() {
  assert string_tree.is_equal(
    string_tree.from_string("12"),
    string_tree.from_strings(["1", "2"]),
  )
}

pub fn is_equal_same_structure_test() {
  assert string_tree.is_equal(
    string_tree.from_string("12"),
    string_tree.from_string("12"),
  )
}

pub fn is_equal_different_content_test() {
  assert !string_tree.is_equal(
    string_tree.from_string("12"),
    string_tree.from_string("2"),
  )
}

pub fn is_empty_empty_string_test() {
  assert string_tree.is_empty(string_tree.from_string(""))
}

pub fn is_empty_non_empty_test() {
  assert !string_tree.is_empty(string_tree.from_string("12"))
}

pub fn is_empty_empty_list_test() {
  assert string_tree.is_empty(string_tree.from_strings([]))
}

pub fn is_empty_list_of_empty_strings_test() {
  assert string_tree.is_empty(string_tree.from_strings(["", ""]))
}

pub fn new_test() {
  assert string_tree.to_string(string_tree.new()) == ""
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

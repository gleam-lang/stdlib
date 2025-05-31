import gleam/bytes_tree
import gleam/string_tree

pub fn tree_test() {
  let data =
    bytes_tree.from_bit_array(<<1>>)
    |> bytes_tree.append(<<2>>)
    |> bytes_tree.append(<<3>>)
    |> bytes_tree.prepend(<<0>>)

  assert data
    |> bytes_tree.to_bit_array
    == <<0, 1, 2, 3>>

  assert data
    |> bytes_tree.byte_size
    == 4
}

pub fn tree_unaligned_bit_arrays_test() {
  let data =
    bytes_tree.from_bit_array(<<-1:5>>)
    |> bytes_tree.append(<<-1:3>>)
    |> bytes_tree.append(<<-2:2>>)
    |> bytes_tree.prepend(<<-1:4>>)

  assert data
    |> bytes_tree.to_bit_array
    == <<-1:4, 0:4, -1:5, 0:3, -1:3, 0:5, -2:2, 0:6>>

  assert data
    |> bytes_tree.byte_size
    == 4
}

pub fn tree_with_strings_test() {
  let data =
    bytes_tree.from_bit_array(<<1>>)
    |> bytes_tree.append_string("2")
    |> bytes_tree.append_string("3")
    |> bytes_tree.prepend_string("0")

  assert data
    |> bytes_tree.to_bit_array
    == <<"0":utf8, 1, "2":utf8, "3":utf8>>

  assert data
    |> bytes_tree.byte_size
    == 4
}

pub fn tree_with_trees_test() {
  let data =
    bytes_tree.from_bit_array(<<1>>)
    |> bytes_tree.append_tree(bytes_tree.from_bit_array(<<2>>))
    |> bytes_tree.append_tree(bytes_tree.from_bit_array(<<3>>))
    |> bytes_tree.prepend_tree(bytes_tree.from_bit_array(<<0>>))

  assert data
    |> bytes_tree.to_bit_array
    == <<0, 1, 2, 3>>

  assert data
    |> bytes_tree.byte_size
    == 4
}

pub fn concat_test() {
  assert [
      bytes_tree.from_bit_array(<<1, 2>>),
      bytes_tree.from_bit_array(<<3, 4>>),
      bytes_tree.from_bit_array(<<5, 6>>),
    ]
    |> bytes_tree.concat
    |> bytes_tree.to_bit_array
    == <<1, 2, 3, 4, 5, 6>>
}

pub fn concat_bit_arrays_test() {
  assert bytes_tree.concat_bit_arrays([<<"h":utf8>>, <<"e":utf8>>, <<"y":utf8>>])
    |> bytes_tree.to_bit_array
    == <<"hey":utf8>>
}

pub fn concat_unaligned_bit_arrays_test() {
  assert bytes_tree.concat_bit_arrays([<<-1:4>>, <<-1:5>>, <<-1:3>>, <<-2:2>>])
    |> bytes_tree.to_bit_array
    == <<-1:4, 0:4, -1:5, 0:3, -1:3, 0:5, -2:2, 0:6>>
}

pub fn from_bit_array() {
  // Regression test: no additional modification of the tree
  assert bytes_tree.from_bit_array(<<>>)
    |> bytes_tree.to_bit_array
    == <<>>
}

pub fn from_string_test() {
  // Regression test: no additional modification of the tree
  assert bytes_tree.from_string("")
    |> bytes_tree.to_bit_array
    == <<>>
}

pub fn new_test() {
  assert bytes_tree.new()
    |> bytes_tree.to_bit_array
    == <<>>
}

pub fn from_string_tree_test() {
  assert string_tree.from_string("hello")
    |> bytes_tree.from_string_tree
    == bytes_tree.from_string("hello")
}

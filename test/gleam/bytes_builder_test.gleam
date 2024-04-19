import gleam/bytes_builder
import gleam/should
import gleam/string_builder

pub fn builder_test() {
  let data =
    bytes_builder.from_bit_array(<<1>>)
    |> bytes_builder.append(<<2>>)
    |> bytes_builder.append(<<3>>)
    |> bytes_builder.prepend(<<0>>)

  data
  |> bytes_builder.to_bit_array
  |> should.equal(<<0, 1, 2, 3>>)

  data
  |> bytes_builder.byte_size
  |> should.equal(4)
}

pub fn builder_with_strings_test() {
  let data =
    bytes_builder.from_bit_array(<<1>>)
    |> bytes_builder.append_string("2")
    |> bytes_builder.append_string("3")
    |> bytes_builder.prepend_string("0")

  data
  |> bytes_builder.to_bit_array
  |> should.equal(<<"0":utf8, 1, "2":utf8, "3":utf8>>)

  data
  |> bytes_builder.byte_size
  |> should.equal(4)
}

pub fn builder_with_builders_test() {
  let data =
    bytes_builder.from_bit_array(<<1>>)
    |> bytes_builder.append_builder(bytes_builder.from_bit_array(<<2>>))
    |> bytes_builder.append_builder(bytes_builder.from_bit_array(<<3>>))
    |> bytes_builder.prepend_builder(bytes_builder.from_bit_array(<<0>>))

  data
  |> bytes_builder.to_bit_array
  |> should.equal(<<0, 1, 2, 3>>)

  data
  |> bytes_builder.byte_size
  |> should.equal(4)
}

pub fn concat_test() {
  [
    bytes_builder.from_bit_array(<<1, 2>>),
    bytes_builder.from_bit_array(<<3, 4>>),
    bytes_builder.from_bit_array(<<5, 6>>),
  ]
  |> bytes_builder.concat
  |> bytes_builder.to_bit_array
  |> should.equal(<<1, 2, 3, 4, 5, 6>>)
}

pub fn concat_bit_arrays_test() {
  bytes_builder.concat_bit_arrays([<<"h":utf8>>, <<"e":utf8>>, <<"y":utf8>>])
  |> bytes_builder.to_bit_array
  |> should.equal(<<"hey":utf8>>)
}

pub fn from_bit_array() {
  // Regression test: no additional modification of the builder
  bytes_builder.from_bit_array(<<>>)
  |> bytes_builder.to_bit_array
  |> should.equal(<<>>)
}

pub fn from_string_test() {
  // Regression test: no additional modification of the builder
  bytes_builder.from_string("")
  |> bytes_builder.to_bit_array
  |> should.equal(<<>>)
}

pub fn new_test() {
  bytes_builder.new()
  |> bytes_builder.to_bit_array
  |> should.equal(<<>>)
}

pub fn from_string_builder_test() {
  string_builder.from_string("hello")
  |> bytes_builder.from_string_builder
  |> should.equal(bytes_builder.from_string("hello"))
}

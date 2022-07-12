import gleam/bit_builder
import gleam/string_builder
import gleam/should

pub fn builder_test() {
  let data =
    bit_builder.from_bit_string(<<1>>)
    |> bit_builder.append(<<2>>)
    |> bit_builder.append(<<3>>)
    |> bit_builder.prepend(<<0>>)

  data
  |> bit_builder.to_bit_string
  |> should.equal(<<0, 1, 2, 3>>)

  data
  |> bit_builder.byte_size
  |> should.equal(4)
}

pub fn builder_with_strings_test() {
  let data =
    bit_builder.from_bit_string(<<1>>)
    |> bit_builder.append_string("2")
    |> bit_builder.append_string("3")
    |> bit_builder.prepend_string("0")

  data
  |> bit_builder.to_bit_string
  |> should.equal(<<"0":utf8, 1, "2":utf8, "3":utf8>>)

  data
  |> bit_builder.byte_size
  |> should.equal(4)
}

pub fn builder_with_builders_test() {
  let data =
    bit_builder.from_bit_string(<<1>>)
    |> bit_builder.append_builder(bit_builder.from_bit_string(<<2>>))
    |> bit_builder.append_builder(bit_builder.from_bit_string(<<3>>))
    |> bit_builder.prepend_builder(bit_builder.from_bit_string(<<0>>))

  data
  |> bit_builder.to_bit_string
  |> should.equal(<<0, 1, 2, 3>>)

  data
  |> bit_builder.byte_size
  |> should.equal(4)
}

pub fn concat_test() {
  [
    bit_builder.from_bit_string(<<1, 2>>),
    bit_builder.from_bit_string(<<3, 4>>),
    bit_builder.from_bit_string(<<5, 6>>),
  ]
  |> bit_builder.concat
  |> bit_builder.to_bit_string
  |> should.equal(<<1, 2, 3, 4, 5, 6>>)
}

pub fn concat_bit_strings_test() {
  bit_builder.concat_bit_strings([<<"h":utf8>>, <<"e":utf8>>, <<"y":utf8>>])
  |> bit_builder.to_bit_string
  |> should.equal(<<"hey":utf8>>)
}

pub fn from_bit_string_test() {
  // Regression test: no additional modification of the builder
  bit_builder.from_bit_string(<<>>)
  |> bit_builder.to_bit_string
  |> should.equal(<<>>)
}

pub fn from_string_test() {
  // Regression test: no additional modification of the builder
  bit_builder.from_string("")
  |> bit_builder.to_bit_string
  |> should.equal(<<>>)
}

pub fn new_test() {
  bit_builder.new()
  |> bit_builder.to_bit_string
  |> should.equal(<<>>)
}

pub fn from_string_builder_test() {
  string_builder.from_string("hello")
  |> bit_builder.from_string_builder
  |> should.equal(bit_builder.from_string("hello"))
}

import gleam/bit_array
import gleam/should

pub fn byte_size_test() {
  bit_array.byte_size(bit_array.from_string("hello"))
  |> should.equal(5)

  bit_array.byte_size(bit_array.from_string(""))
  |> should.equal(0)
}

pub fn not_equal_test() {
  bit_array.from_string("test")
  |> should.not_equal(bit_array.from_string("asdf"))
}

pub fn append_test() {
  bit_array.from_string("Test")
  |> bit_array.append(bit_array.from_string(" Me"))
  |> should.equal(bit_array.from_string("Test Me"))

  <<1, 2>>
  |> bit_array.append(<<>>)
  |> should.equal(<<1, 2>>)

  <<1, 2>>
  |> bit_array.append(<<3, 4>>)
  |> should.equal(<<1, 2, 3, 4>>)
}

@target(erlang)
pub fn append_erlang_only_test() {
  <<1, 2:4>>
  |> bit_array.append(<<3>>)
  |> should.equal(<<1, 2:4, 3>>)
}

pub fn concat_test() {
  [<<1, 2>>]
  |> bit_array.concat
  |> should.equal(<<1, 2>>)

  [<<1, 2>>, <<3>>, <<4>>]
  |> bit_array.concat
  |> should.equal(<<1, 2, 3, 4>>)
}

@target(erlang)
pub fn concat_erlang_only_test() {
  [<<1, 2:4>>, <<3>>]
  |> bit_array.concat
  |> should.equal(<<1, 2:4, 3>>)
}

pub fn slice_test() {
  <<"hello":utf8>>
  |> bit_array.slice(0, 5)
  |> should.equal(Ok(<<"hello":utf8>>))

  <<"hello":utf8>>
  |> bit_array.slice(0, 0)
  |> should.equal(Ok(<<"":utf8>>))

  <<"hello":utf8>>
  |> bit_array.slice(2, 2)
  |> should.equal(Ok(<<"ll":utf8>>))

  <<"hello":utf8>>
  |> bit_array.slice(5, -2)
  |> should.equal(Ok(<<"lo":utf8>>))

  <<"":utf8>>
  |> bit_array.slice(0, 0)
  |> should.equal(Ok(<<"":utf8>>))

  <<"hello":utf8>>
  |> bit_array.slice(6, 0)
  |> should.equal(Error(Nil))

  <<"hello":utf8>>
  |> bit_array.slice(1, -2)
  |> should.equal(Error(Nil))

  bit_array.from_string("hello")
  |> bit_array.slice(-1, 1)
  |> should.equal(Error(Nil))

  bit_array.from_string("hello")
  |> bit_array.slice(1, 6)
  |> should.equal(Error(Nil))
}

pub fn to_string_test() {
  <<>>
  |> bit_array.to_string
  |> should.equal(Ok(""))

  <<"":utf8>>
  |> bit_array.to_string
  |> should.equal(Ok(""))

  <<"Hello":utf8>>
  |> bit_array.to_string
  |> should.equal(Ok("Hello"))

  <<"ø":utf8>>
  |> bit_array.to_string
  |> should.equal(Ok("ø"))

  <<65_535>>
  |> bit_array.to_string
  |> should.equal(Error(Nil))
}

pub fn is_utf8_test() {
  <<>>
  |> bit_array.is_utf8
  |> should.be_true

  <<"":utf8>>
  |> bit_array.is_utf8
  |> should.be_true

  <<"Hello":utf8>>
  |> bit_array.is_utf8
  |> should.be_true

  <<"ø":utf8>>
  |> bit_array.is_utf8
  |> should.be_true

  <<65_535>>
  |> bit_array.is_utf8
  |> should.be_false
}

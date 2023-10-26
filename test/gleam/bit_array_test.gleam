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

pub fn base64_encode_test() {
  <<255, 127, 254, 252>>
  |> bit_array.base64_encode(True)
  |> should.equal("/3/+/A==")

  <<255, 127, 254, 252>>
  |> bit_array.base64_encode(False)
  |> should.equal("/3/+/A")

  <<0, 0, 0>>
  |> bit_array.base64_encode(True)
  |> should.equal("AAAA")

  <<>>
  |> bit_array.base64_encode(True)
  |> should.equal("")
}

pub fn base64_decode_test() {
  "/3/+/A=="
  |> bit_array.base64_decode()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "/3/+/A"
  |> bit_array.base64_decode()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "AAAA"
  |> bit_array.base64_decode()
  |> should.equal(Ok(<<0, 0, 0>>))

  ""
  |> bit_array.base64_decode()
  |> should.equal(Ok(<<>>))

  ")!"
  |> bit_array.base64_decode()
  |> should.equal(Error(Nil))
}

pub fn base64_url_encode_test() {
  <<255, 127, 254, 252>>
  |> bit_array.base64_url_encode(True)
  |> should.equal("_3_-_A==")

  <<255, 127, 254, 252>>
  |> bit_array.base64_url_encode(False)
  |> should.equal("_3_-_A")

  <<0, 0, 0>>
  |> bit_array.base64_url_encode(True)
  |> should.equal("AAAA")

  <<>>
  |> bit_array.base64_url_encode(True)
  |> should.equal("")
}

pub fn base64_url_decode_test() {
  "_3_-_A=="
  |> bit_array.base64_url_decode()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "_3_-_A"
  |> bit_array.base64_url_decode()
  |> should.equal(Ok(<<255, 127, 254, 252>>))

  "AAAA"
  |> bit_array.base64_url_decode()
  |> should.equal(Ok(<<0, 0, 0>>))

  ""
  |> bit_array.base64_url_decode()
  |> should.equal(Ok(<<>>))

  ")!"
  |> bit_array.base64_url_decode()
  |> should.equal(Error(Nil))
}

pub fn decode64_crash_regression_1_test() {
  "aGktdGhlcmU.uWUWvrAleKQ2jsWcU97H-RPJ5qRRcE_s"
  |> bit_array.base64_decode()
  |> should.equal(Error(Nil))
}

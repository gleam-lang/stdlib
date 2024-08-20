import gleam/bit_array
import gleam/order
import gleam/result
import gleam/should
import gleam/string

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

// This test is target specific since it's using non byte-aligned BitArrays
// and those are not supported on the JavaScript target.
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

// This test is target specific since it's using non byte-aligned BitArrays
// and those are not supported on the JavaScript target.
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

  bit_array.from_string("ab")
  |> bit_array.slice(1, 1)
  |> result.try(bit_array.slice(_, 0, 1))
  |> should.equal(Ok(<<"b":utf8>>))
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

  <<255, 127, 254, 252, 100>>
  |> bit_array.base64_encode(True)
  |> should.equal("/3/+/GQ=")

  <<255, 127, 254, 252>>
  |> bit_array.base64_encode(False)
  |> should.equal("/3/+/A")

  <<0, 0, 0>>
  |> bit_array.base64_encode(True)
  |> should.equal("AAAA")

  <<>>
  |> bit_array.base64_encode(True)
  |> should.equal("")

  string.repeat("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 1024 * 32)
  |> bit_array.from_string
  |> bit_array.base64_encode(True)
  |> should.equal(string.repeat(
    "QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB",
    1024 * 32,
  ))
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

  "=AAA"
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

pub fn base16_test() {
  bit_array.base16_encode(<<"":utf8>>)
  |> should.equal("")

  bit_array.base16_encode(<<"f":utf8>>)
  |> should.equal("66")

  bit_array.base16_encode(<<"fo":utf8>>)
  |> should.equal("666F")

  bit_array.base16_encode(<<"foo":utf8>>)
  |> should.equal("666F6F")

  bit_array.base16_encode(<<"foob":utf8>>)
  |> should.equal("666F6F62")

  bit_array.base16_encode(<<"fooba":utf8>>)
  |> should.equal("666F6F6261")

  bit_array.base16_encode(<<"foobar":utf8>>)
  |> should.equal("666F6F626172")

  bit_array.base16_encode(<<161, 178, 195, 212, 229, 246, 120, 145>>)
  |> should.equal("A1B2C3D4E5F67891")
}

pub fn base16_decode_test() {
  bit_array.base16_decode("")
  |> should.equal(Ok(<<>>))

  bit_array.base16_decode("66")
  |> should.equal(Ok(<<"f":utf8>>))

  bit_array.base16_decode("666F")
  |> should.equal(Ok(<<"fo":utf8>>))

  bit_array.base16_decode("666F6F")
  |> should.equal(Ok(<<"foo":utf8>>))

  bit_array.base16_decode("666F6F62")
  |> should.equal(Ok(<<"foob":utf8>>))

  bit_array.base16_decode("666F6F6261")
  |> should.equal(Ok(<<"fooba":utf8>>))

  bit_array.base16_decode("666F6F626172")
  |> should.equal(Ok(<<"foobar":utf8>>))

  bit_array.base16_decode("A1B2C3D4E5F67891")
  |> should.equal(Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>))

  // Not a hex string
  bit_array.base16_decode("?")
  |> should.equal(Error(Nil))

  // Lowercase hex
  bit_array.base16_decode("a1b2c3d4e5f67891")
  |> should.equal(Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>))
}

pub fn inspect_test() {
  bit_array.inspect(<<>>)
  |> should.equal("<<>>")

  bit_array.inspect(<<80>>)
  |> should.equal("<<80>>")

  bit_array.inspect(<<0, 20, 0x20, 255>>)
  |> should.equal("<<0, 20, 32, 255>>")
}

// This test is target specific since it's using non byte-aligned BitArrays
// and those are not supported on the JavaScript target.
@target(erlang)
pub fn inspect_partial_bytes_test() {
  bit_array.inspect(<<4:5>>)
  |> should.equal("<<4:size(5)>>")

  bit_array.inspect(<<100, 5:3>>)
  |> should.equal("<<100, 5:size(3)>>")

  bit_array.inspect(<<5:3, 11:4, 1:2>>)
  |> should.equal("<<182, 1:size(1)>>")
}

@target(erlang)
pub fn compare_test() {
  bit_array.compare(<<1, 2, 3>>, <<1, 2, 3>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<1, 2>>, <<1, 3>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<1, 3>>, <<1, 2>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<1, 2>>, <<1, 2, 3>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<1, 2, 3>>, <<1, 2>>)
  |> should.equal(order.Gt)
}

pub fn compare_utf8_test() {
  bit_array.compare(<<"ABC":utf8>>, <<"ABC":utf8>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"AB":utf8>>, <<"ABC":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"ABC":utf8>>, <<"AB":utf8>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"AB":utf8>>, <<"AC":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"AC":utf8>>, <<"AB":utf8>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"":utf8>>, <<"ABC":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"A":utf8>>, <<"":utf8>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"":utf8>>, <<"":utf8>>)
  |> should.equal(order.Eq)
}

pub fn compare_utf8_and_numbers_test() {
  bit_array.compare(<<"A":utf8>>, <<65>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"A":utf8>>, <<64>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"A":utf8>>, <<66>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"AA":utf8>>, <<65, 65>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"AAA":utf8>>, <<65, 65>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"AA":utf8>>, <<65, 65, 1>>)
  |> should.equal(order.Lt)
}

@target(erlang)
pub fn compare_utf16_test() {
  bit_array.compare(<<"ABC":utf16>>, <<"ABC":utf16>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"ABC":utf16>>, <<"AB":utf16>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"A":utf16>>, <<"Z":utf16>>)
  |> should.equal(order.Lt)
}

@target(erlang)
pub fn compare_utf32_test() {
  bit_array.compare(<<"ABC":utf32>>, <<"ABC":utf32>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"ABC":utf32>>, <<"AB":utf32>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"A":utf32>>, <<"Z":utf32>>)
  |> should.equal(order.Lt)
}

@target(erlang)
pub fn compare_mixed_utfs_test() {
  bit_array.compare(<<"A":utf16>>, <<"A":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"A":utf32>>, <<"A":utf16>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"A":utf8>>, <<"A":utf16>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<"":utf8>>, <<"A":utf16>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"":utf16>>, <<"A":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"":utf32>>, <<"A":utf8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"":utf32>>, <<"A":utf16>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<"":utf16>>, <<"":utf8>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"":utf8>>, <<"":utf16>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<"":utf8>>, <<"":utf32>>)
  |> should.equal(order.Eq)
}

@target(erlang)
pub fn compare_different_sizes_test() {
  bit_array.compare(<<4:5>>, <<4:5>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<4:5, 3:3>>, <<4:5, 2:3>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<4:5, 3:3>>, <<4:5, 4:3>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<4:5, 3:3, 0:0>>, <<4:5, 3:3, 0:0>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<3:5>>, <<4:5>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<3:7>>, <<4:7>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<5:5>>, <<4:5>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<4:8>>, <<4:5>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<4:5>>, <<4:8>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<0:5>>, <<0:8>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<0:2>>, <<0:1>>)
  |> should.equal(order.Eq)
}

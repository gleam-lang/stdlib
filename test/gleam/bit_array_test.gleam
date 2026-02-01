import gleam/bit_array
import gleam/order
import gleam/result
import gleam/string

pub fn bit_size_test() {
  assert bit_array.bit_size(<<>>) == 0

  assert bit_array.bit_size(<<0>>) == 8

  assert bit_array.bit_size(<<-1:32>>) == 32

  assert bit_array.bit_size(<<0:-8>>) == 0

  assert bit_array.bit_size(<<0:1>>) == 1

  assert bit_array.bit_size(<<7:3>>) == 3

  assert bit_array.bit_size(<<-1:190>>) == 190

  assert bit_array.bit_size(<<0:-1>>) == 0
}

pub fn byte_size_test() {
  assert bit_array.byte_size(<<>>) == 0

  assert bit_array.byte_size(<<0, 1, 2, 3, 4>>) == 5

  assert bit_array.byte_size(<<1, 2, 3:6>>) == 3
}

pub fn pad_to_bytes_test() {
  assert bit_array.pad_to_bytes(<<>>) == <<>>

  assert bit_array.pad_to_bytes(<<0xAB>>) == <<0xAB>>

  assert bit_array.pad_to_bytes(<<0xAB, 0x12>>) == <<0xAB, 0x12>>

  assert bit_array.pad_to_bytes(<<1:1>>) == <<0x80>>

  assert bit_array.pad_to_bytes(<<-1:7>>) == <<0xFE>>

  assert bit_array.pad_to_bytes(<<0xAB, 0x12, 3:3>>) == <<0xAB, 0x12, 0x60>>

  let assert <<a:bits-12, _:4>> = <<0xAB, 0xFF>>
  assert bit_array.pad_to_bytes(a) == <<0xAB, 0xF0>>
}

pub fn not_equal_test() {
  assert bit_array.from_string("test") != bit_array.from_string("asdf")
}

pub fn append_test() {
  assert bit_array.append(
      bit_array.from_string("Test"),
      bit_array.from_string(" Me"),
    )
    == bit_array.from_string("Test Me")

  assert bit_array.append(<<1, 2>>, <<>>) == <<1, 2>>

  assert bit_array.append(<<1, 2>>, <<3, 4>>) == <<1, 2, 3, 4>>

  assert bit_array.append(<<1, 2:4>>, <<3>>) == <<1, 2:4, 3>>
}

pub fn concat_test() {
  assert bit_array.concat([<<1, 2>>]) == <<1, 2>>

  assert bit_array.concat([<<1, 2>>, <<3>>, <<4>>]) == <<1, 2, 3, 4>>

  assert bit_array.concat([<<-1:32>>, <<0:1>>, <<0:0>>])
    == <<255, 255, 255, 255, 0:1>>

  assert bit_array.concat([<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>])
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>

  assert bit_array.concat([<<1, 2:4>>, <<3>>]) == <<1, 2:4, 3>>

  assert bit_array.concat([<<-1:32>>, <<0:1>>, <<0:0>>])
    == <<255, 255, 255, 255, 0:1>>

  assert bit_array.concat([<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>])
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>
}

pub fn slice_test() {
  assert bit_array.slice(<<"hello":utf8>>, 0, 5) == Ok(<<"hello":utf8>>)

  assert bit_array.slice(<<"hello":utf8>>, 0, 0) == Ok(<<"":utf8>>)

  assert bit_array.slice(<<"hello":utf8>>, 2, 2) == Ok(<<"ll":utf8>>)

  assert bit_array.slice(<<"hello":utf8>>, 5, -2) == Ok(<<"lo":utf8>>)

  assert bit_array.slice(<<"":utf8>>, 0, 0) == Ok(<<"":utf8>>)

  assert bit_array.slice(<<"hello":utf8>>, 6, 0) == Error(Nil)

  assert bit_array.slice(<<"hello":utf8>>, 1, -2) == Error(Nil)

  assert bit_array.slice(bit_array.from_string("hello"), -1, 1) == Error(Nil)

  assert bit_array.slice(bit_array.from_string("hello"), 1, 6) == Error(Nil)

  assert bit_array.from_string("ab")
    |> bit_array.slice(1, 1)
    |> result.try(bit_array.slice(_, 0, 1))
    == Ok(<<"b":utf8>>)

  assert bit_array.slice(<<0, 1, 2:7>>, 0, 3) == Error(Nil)

  assert bit_array.slice(
      <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
      8,
      12,
    )
    == Error(Nil)
}

pub fn split_once_test() {
  <<"hello":utf8>>
  |> bit_array.split_once(<<"l":utf8>>)
  |> should.equal(Ok(#(<<"he":utf8>>, <<"lo":utf8>>)))

  <<"hello":utf8>>
  |> bit_array.split_once(<<"o":utf8>>)
  |> should.equal(Ok(#(<<"hell":utf8>>, <<>>)))

  <<"hello":utf8>>
  |> bit_array.split_once(<<"h":utf8>>)
  |> should.equal(Ok(#(<<>>, <<"ello":utf8>>)))

  <<0, 1, 0, 2, 0, 3>>
  |> bit_array.split_once(<<0, 2>>)
  |> should.equal(Ok(#(<<0, 1>>, <<0, 3>>)))

  <<0, 1, 2, 0, 3, 4, 5>>
  |> bit_array.split_once(<<>>)
  |> should.equal(Error(Nil))

  <<>>
  |> bit_array.split_once(<<1>>)
  |> should.equal(Error(Nil))

  <<1>>
  |> bit_array.split_once(<<1>>)
  |> should.equal(Error(Nil))

  <<0>>
  |> bit_array.split_once(<<1>>)
  |> should.equal(Error(Nil))
}

// This test is target specific since it's using non byte-aligned BitArrays
// and those are not supported on the JavaScript target.
@target(erlang)
pub fn split_once_erlang_only_test() {
  <<0, 1, 2:7>>
  |> bit_array.split_once(<<1>>)
  |> should.equal(Error(Nil))
}

pub fn split_test() {
  <<"hello":utf8>>
  |> bit_array.split(<<"l":utf8>>)
  |> should.equal(Ok([<<"he":utf8>>, <<"o":utf8>>]))

  <<0, 1, 0, 2, 0, 3>>
  |> bit_array.split(<<0>>)
  |> should.equal(Ok([<<1>>, <<2>>, <<3>>]))

  <<0, 1, 0, 2, 0, 3>>
  |> bit_array.split(<<0, 2>>)
  |> should.equal(Ok([<<0, 1>>, <<0, 3>>]))

  <<1, 0>>
  |> bit_array.split(<<0>>)
  |> should.equal(Ok([<<1>>]))

  <<1, 0>>
  |> bit_array.split(<<1>>)
  |> should.equal(Ok([<<0>>]))

  <<1>>
  |> bit_array.split(<<0>>)
  |> should.equal(Ok([<<1>>]))

  <<1, 2>>
  |> bit_array.split(<<1, 2>>)
  |> should.equal(Ok([]))

  <<0, 1, 2, 0, 3, 4, 5>>
  |> bit_array.split(<<>>)
  |> should.equal(Error(Nil))
}

// This test is target specific since it's using non byte-aligned BitArrays
// and those are not supported on the JavaScript target.
@target(erlang)
pub fn split_erlang_only_test() {
  <<0, 1, 2:7>>
  |> bit_array.split(<<1>>)
  |> should.equal(Error(Nil))
}

pub fn to_string_test() {
  assert bit_array.to_string(<<>>) == Ok("")

  assert bit_array.to_string(<<"":utf8>>) == Ok("")

  assert bit_array.to_string(<<"Hello":utf8>>) == Ok("Hello")

  assert bit_array.to_string(<<"ø":utf8>>) == Ok("ø")

  assert bit_array.to_string(<<255>>) == Error(Nil)

  assert bit_array.to_string(<<"ø":utf8, 2:4>>) == Error(Nil)

  let assert <<_:3, x:bits>> = <<0:3, "ø":utf8>>
  assert bit_array.to_string(x) == Ok("ø")
}

pub fn is_utf8_test() {
  assert bit_array.is_utf8(<<>>)

  assert bit_array.is_utf8(<<"":utf8>>)

  assert bit_array.is_utf8(<<"Hello":utf8>>)

  assert bit_array.is_utf8(<<"ø":utf8>>)

  assert !bit_array.is_utf8(<<255>>)
}

pub fn base64_encode_test() {
  assert bit_array.base64_encode(<<255, 127, 254, 252>>, True) == "/3/+/A=="

  assert bit_array.base64_encode(<<255, 127, 254, 252, 100>>, True)
    == "/3/+/GQ="

  assert bit_array.base64_encode(<<255, 127, 254, 252>>, False) == "/3/+/A"

  assert bit_array.base64_encode(<<0, 0, 0>>, True) == "AAAA"

  assert bit_array.base64_encode(<<>>, True) == ""

  assert string.repeat("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 1024 * 32)
    |> bit_array.from_string
    |> bit_array.base64_encode(True)
    == string.repeat("QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB", 1024 * 32)

  assert bit_array.base64_encode(<<-1:7>>, True) == "/g=="

  assert bit_array.base64_encode(<<0xFA, 5:3>>, True) == "+qA="

  assert bit_array.base64_encode(<<0xFA, 0xBC, 0x6D, 1:1>>, True) == "+rxtgA=="
}

pub fn base64_decode_test() {
  assert bit_array.base64_decode("/3/+/A==") == Ok(<<255, 127, 254, 252>>)

  assert bit_array.base64_decode("/3/+/A") == Ok(<<255, 127, 254, 252>>)

  assert bit_array.base64_decode("AAAA") == Ok(<<0, 0, 0>>)

  assert bit_array.base64_decode("") == Ok(<<>>)

  assert bit_array.base64_decode(")!") == Error(Nil)

  assert bit_array.base64_decode("=AAA") == Error(Nil)
}

pub fn base64_url_encode_test() {
  assert bit_array.base64_url_encode(<<255, 127, 254, 252>>, True) == "_3_-_A=="

  assert bit_array.base64_url_encode(<<255, 127, 254, 252>>, False) == "_3_-_A"

  assert bit_array.base64_url_encode(<<0, 0, 0>>, True) == "AAAA"

  assert bit_array.base64_url_encode(<<>>, True) == ""
}

pub fn base64_url_decode_test() {
  assert bit_array.base64_url_decode("_3_-_A==") == Ok(<<255, 127, 254, 252>>)

  assert bit_array.base64_url_decode("_3_-_A") == Ok(<<255, 127, 254, 252>>)

  assert bit_array.base64_url_decode("AAAA") == Ok(<<0, 0, 0>>)

  assert bit_array.base64_url_decode("") == Ok(<<>>)

  assert bit_array.base64_url_decode(")!") == Error(Nil)
}

pub fn base64_decode_crash_regression_1_test() {
  assert bit_array.base64_decode("aGktdGhlcmU.uWUWvrAleKQ2jsWcU97H-RPJ5qRRcE_s")
    == Error(Nil)
}

pub fn base16_encode_test() {
  assert bit_array.base16_encode(<<"":utf8>>) == ""

  assert bit_array.base16_encode(<<"f":utf8>>) == "66"

  assert bit_array.base16_encode(<<"fo":utf8>>) == "666F"

  assert bit_array.base16_encode(<<"foo":utf8>>) == "666F6F"

  assert bit_array.base16_encode(<<"foob":utf8>>) == "666F6F62"

  assert bit_array.base16_encode(<<"fooba":utf8>>) == "666F6F6261"

  assert bit_array.base16_encode(<<"foobar":utf8>>) == "666F6F626172"

  assert bit_array.base16_encode(<<161, 178, 195, 212, 229, 246, 120, 145>>)
    == "A1B2C3D4E5F67891"

  assert bit_array.base16_encode(<<-1:7>>) == "FE"

  assert bit_array.base16_encode(<<0xFA, 5:3>>) == "FAA0"

  assert bit_array.base16_encode(<<0xFA, 5:4>>) == "FA50"

  assert bit_array.base16_encode(<<0xFA, 0xBC, 0x6D, 1:1>>) == "FABC6D80"
}

pub fn base16_decode_test() {
  assert bit_array.base16_decode("") == Ok(<<>>)

  assert bit_array.base16_decode("66") == Ok(<<"f":utf8>>)

  assert bit_array.base16_decode("666F") == Ok(<<"fo":utf8>>)

  assert bit_array.base16_decode("666F6F") == Ok(<<"foo":utf8>>)

  assert bit_array.base16_decode("666F6F62") == Ok(<<"foob":utf8>>)

  assert bit_array.base16_decode("666F6F6261") == Ok(<<"fooba":utf8>>)

  assert bit_array.base16_decode("666F6F626172") == Ok(<<"foobar":utf8>>)

  assert bit_array.base16_decode("A1B2C3D4E5F67891")
    == Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>)

  // Not a hex string
  assert bit_array.base16_decode("?") == Error(Nil)

  // Lowercase hex
  assert bit_array.base16_decode("a1b2c3d4e5f67891")
    == Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>)
}

pub fn inspect_test() {
  assert bit_array.inspect(<<>>) == "<<>>"

  assert bit_array.inspect(<<80>>) == "<<80>>"

  assert bit_array.inspect(<<0, 20, 0x20, 255>>) == "<<0, 20, 32, 255>>"

  assert bit_array.inspect(<<4:5>>) == "<<4:size(5)>>"

  assert bit_array.inspect(<<100, 5:3>>) == "<<100, 5:size(3)>>"

  assert bit_array.inspect(<<5:3, 11:4, 1:2>>) == "<<182, 1:size(1)>>"
}

pub fn compare_test() {
  assert bit_array.compare(<<4:5>>, <<4:5>>) == order.Eq

  assert bit_array.compare(<<4:5, 3:3>>, <<4:5, 2:3>>) == order.Gt

  assert bit_array.compare(<<4:5, 3:3>>, <<4:5, 4:3>>) == order.Lt

  assert bit_array.compare(<<4:5, 3:3, 0:0>>, <<4:5, 3:3, 0:0>>) == order.Eq

  assert bit_array.compare(<<0:2, 3:4, 0:0>>, <<0:2, 3:3, 0:0>>) == order.Gt

  // first is: <<33, 1:size(1)>>
  // second is: <<35>>
  assert bit_array.compare(<<4:5, 3:4, 0:0>>, <<4:5, 3:3, 0:0>>) == order.Lt

  assert bit_array.compare(<<3:5>>, <<4:5>>) == order.Lt

  assert bit_array.compare(<<3:7>>, <<4:7>>) == order.Lt

  assert bit_array.compare(<<5:5>>, <<4:5>>) == order.Gt

  assert bit_array.compare(<<4:8>>, <<4:5>>) == order.Gt

  assert bit_array.compare(<<4:5>>, <<4:8>>) == order.Lt

  assert bit_array.compare(<<0:5>>, <<0:8>>) == order.Lt

  assert bit_array.compare(<<0:5>>, <<0:5>>) == order.Eq

  assert bit_array.compare(<<0:2>>, <<0:1>>) == order.Gt
}

pub fn starts_with_test() {
  assert bit_array.starts_with(<<>>, <<>>)

  assert bit_array.starts_with(<<0>>, <<>>)

  assert !bit_array.starts_with(<<>>, <<0>>)

  assert bit_array.starts_with(<<0, 1, 2>>, <<0>>)

  assert bit_array.starts_with(<<0, 1, 2>>, <<0, 1>>)

  assert bit_array.starts_with(<<0, 1, 2>>, <<0, 1, 2>>)

  assert !bit_array.starts_with(<<0, 1, 2>>, <<0, 1, 2, 3>>)

  assert !bit_array.starts_with(<<0, 1, 2>>, <<1>>)

  assert bit_array.starts_with(<<1:1>>, <<1:1>>)

  assert bit_array.starts_with(<<1:1>>, <<>>)

  assert !bit_array.starts_with(<<1:1>>, <<1:2>>)

  assert bit_array.starts_with(<<-1:127>>, <<-1:33>>)

  assert !bit_array.starts_with(<<-1:127>>, <<-1:128>>)

  assert !bit_array.starts_with(<<0:127>>, <<1:127>>)

  assert bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 1:1>>)

  assert !bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 0:1>>)
}

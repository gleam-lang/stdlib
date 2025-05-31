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
  assert <<>>
    |> bit_array.pad_to_bytes
    == <<>>

  assert <<0xAB>>
    |> bit_array.pad_to_bytes
    == <<0xAB>>

  assert <<0xAB, 0x12>>
    |> bit_array.pad_to_bytes
    == <<0xAB, 0x12>>

  assert <<1:1>>
    |> bit_array.pad_to_bytes
    == <<0x80>>

  assert <<-1:7>>
    |> bit_array.pad_to_bytes
    == <<0xFE>>

  assert <<0xAB, 0x12, 3:3>>
    |> bit_array.pad_to_bytes
    == <<0xAB, 0x12, 0x60>>

  let assert <<a:bits-12, _:4>> = <<0xAB, 0xFF>>
  assert a
    |> bit_array.pad_to_bytes
    == <<0xAB, 0xF0>>
}

pub fn not_equal_test() {
  assert bit_array.from_string("test") != bit_array.from_string("asdf")
}

pub fn append_test() {
  assert bit_array.from_string("Test")
    |> bit_array.append(bit_array.from_string(" Me"))
    == bit_array.from_string("Test Me")

  assert <<1, 2>>
    |> bit_array.append(<<>>)
    == <<1, 2>>

  assert <<1, 2>>
    |> bit_array.append(<<3, 4>>)
    == <<1, 2, 3, 4>>

  assert <<1, 2:4>>
    |> bit_array.append(<<3>>)
    == <<1, 2:4, 3>>
}

pub fn concat_test() {
  assert [<<1, 2>>]
    |> bit_array.concat
    == <<1, 2>>

  assert [<<1, 2>>, <<3>>, <<4>>]
    |> bit_array.concat
    == <<1, 2, 3, 4>>

  assert [<<-1:32>>, <<0:1>>, <<0:0>>]
    |> bit_array.concat
    == <<255, 255, 255, 255, 0:1>>

  assert [<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>]
    |> bit_array.concat
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>

  assert [<<1, 2:4>>, <<3>>]
    |> bit_array.concat
    == <<1, 2:4, 3>>

  assert [<<-1:32>>, <<0:1>>, <<0:0>>]
    |> bit_array.concat
    == <<255, 255, 255, 255, 0:1>>

  assert [<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>]
    |> bit_array.concat
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>
}

pub fn slice_test() {
  assert <<"hello":utf8>>
    |> bit_array.slice(0, 5)
    == Ok(<<"hello":utf8>>)

  assert <<"hello":utf8>>
    |> bit_array.slice(0, 0)
    == Ok(<<"":utf8>>)

  assert <<"hello":utf8>>
    |> bit_array.slice(2, 2)
    == Ok(<<"ll":utf8>>)

  assert <<"hello":utf8>>
    |> bit_array.slice(5, -2)
    == Ok(<<"lo":utf8>>)

  assert <<"":utf8>>
    |> bit_array.slice(0, 0)
    == Ok(<<"":utf8>>)

  assert <<"hello":utf8>>
    |> bit_array.slice(6, 0)
    == Error(Nil)

  assert <<"hello":utf8>>
    |> bit_array.slice(1, -2)
    == Error(Nil)

  assert bit_array.from_string("hello")
    |> bit_array.slice(-1, 1)
    == Error(Nil)

  assert bit_array.from_string("hello")
    |> bit_array.slice(1, 6)
    == Error(Nil)

  assert bit_array.from_string("ab")
    |> bit_array.slice(1, 1)
    |> result.try(bit_array.slice(_, 0, 1))
    == Ok(<<"b":utf8>>)

  assert <<0, 1, 2:7>>
    |> bit_array.slice(0, 3)
    == Error(Nil)

  assert <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>
    |> bit_array.slice(8, 12)
    == Error(Nil)
}

pub fn to_string_test() {
  assert <<>>
    |> bit_array.to_string
    == Ok("")

  assert <<"":utf8>>
    |> bit_array.to_string
    == Ok("")

  assert <<"Hello":utf8>>
    |> bit_array.to_string
    == Ok("Hello")

  assert <<"ø":utf8>>
    |> bit_array.to_string
    == Ok("ø")

  assert <<255>>
    |> bit_array.to_string
    == Error(Nil)

  assert <<"ø":utf8, 2:4>>
    |> bit_array.to_string
    == Error(Nil)

  let assert <<_:3, x:bits>> = <<0:3, "ø":utf8>>
  assert x
    |> bit_array.to_string
    == Ok("ø")
}

pub fn is_utf8_test() {
  assert <<>>
    |> bit_array.is_utf8

  assert <<"":utf8>>
    |> bit_array.is_utf8

  assert <<"Hello":utf8>>
    |> bit_array.is_utf8

  assert <<"ø":utf8>>
    |> bit_array.is_utf8

  assert !{
    <<255>>
    |> bit_array.is_utf8
  }
}

pub fn base64_encode_test() {
  assert <<255, 127, 254, 252>>
    |> bit_array.base64_encode(True)
    == "/3/+/A=="

  assert <<255, 127, 254, 252, 100>>
    |> bit_array.base64_encode(True)
    == "/3/+/GQ="

  assert <<255, 127, 254, 252>>
    |> bit_array.base64_encode(False)
    == "/3/+/A"

  assert <<0, 0, 0>>
    |> bit_array.base64_encode(True)
    == "AAAA"

  assert <<>>
    |> bit_array.base64_encode(True)
    == ""

  assert string.repeat("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 1024 * 32)
    |> bit_array.from_string
    |> bit_array.base64_encode(True)
    == string.repeat("QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB", 1024 * 32)

  assert <<-1:7>>
    |> bit_array.base64_encode(True)
    == "/g=="

  assert <<0xFA, 5:3>>
    |> bit_array.base64_encode(True)
    == "+qA="

  assert <<0xFA, 0xBC, 0x6D, 1:1>>
    |> bit_array.base64_encode(True)
    == "+rxtgA=="
}

pub fn base64_decode_test() {
  assert "/3/+/A=="
    |> bit_array.base64_decode()
    == Ok(<<255, 127, 254, 252>>)

  assert "/3/+/A"
    |> bit_array.base64_decode()
    == Ok(<<255, 127, 254, 252>>)

  assert "AAAA"
    |> bit_array.base64_decode()
    == Ok(<<0, 0, 0>>)

  assert ""
    |> bit_array.base64_decode()
    == Ok(<<>>)

  assert ")!"
    |> bit_array.base64_decode()
    == Error(Nil)

  assert "=AAA"
    |> bit_array.base64_decode()
    == Error(Nil)
}

pub fn base64_url_encode_test() {
  assert <<255, 127, 254, 252>>
    |> bit_array.base64_url_encode(True)
    == "_3_-_A=="

  assert <<255, 127, 254, 252>>
    |> bit_array.base64_url_encode(False)
    == "_3_-_A"

  assert <<0, 0, 0>>
    |> bit_array.base64_url_encode(True)
    == "AAAA"

  assert <<>>
    |> bit_array.base64_url_encode(True)
    == ""
}

pub fn base64_url_decode_test() {
  assert "_3_-_A=="
    |> bit_array.base64_url_decode()
    == Ok(<<255, 127, 254, 252>>)

  assert "_3_-_A"
    |> bit_array.base64_url_decode()
    == Ok(<<255, 127, 254, 252>>)

  assert "AAAA"
    |> bit_array.base64_url_decode()
    == Ok(<<0, 0, 0>>)

  assert ""
    |> bit_array.base64_url_decode()
    == Ok(<<>>)

  assert ")!"
    |> bit_array.base64_url_decode()
    == Error(Nil)
}

pub fn decode64_crash_regression_1_test() {
  assert "aGktdGhlcmU.uWUWvrAleKQ2jsWcU97H-RPJ5qRRcE_s"
    |> bit_array.base64_decode()
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

  assert <<-1:7>>
    |> bit_array.base16_encode()
    == "FE"

  assert <<0xFA, 5:3>>
    |> bit_array.base16_encode()
    == "FAA0"

  assert <<0xFA, 5:4>>
    |> bit_array.base16_encode()
    == "FA50"

  assert <<0xFA, 0xBC, 0x6D, 1:1>>
    |> bit_array.base16_encode()
    == "FABC6D80"
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

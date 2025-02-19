import gleam/bit_array
import gleam/order
import gleam/should

pub fn bit_size_unaligned_test() {
  bit_array.bit_size(<<0:1>>)
  |> should.equal(1)

  bit_array.bit_size(<<7:3>>)
  |> should.equal(3)

  bit_array.bit_size(<<-1:190>>)
  |> should.equal(190)

  bit_array.bit_size(<<0:-1>>)
  |> should.equal(0)
}

pub fn byte_size_unaligned_test() {
  bit_array.byte_size(<<1, 2, 3:6>>)
  |> should.equal(3)
}

pub fn pad_to_bytes_unaligned_test() {
  <<1:1>>
  |> bit_array.pad_to_bytes
  |> should.equal(<<0x80>>)

  <<-1:7>>
  |> bit_array.pad_to_bytes
  |> should.equal(<<0xFE>>)

  <<0xAB, 0x12, 3:3>>
  |> bit_array.pad_to_bytes
  |> should.equal(<<0xAB, 0x12, 0x60>>)

  let assert <<a:bits-12, _:4>> = <<0xAB, 0xFF>>
  a
  |> bit_array.pad_to_bytes
  |> should.equal(<<0xAB, 0xF0>>)
}

pub fn append_unaligned_test() {
  <<1, 2:4>>
  |> bit_array.append(<<3>>)
  |> should.equal(<<1, 2:4, 3>>)
}

pub fn concat_unaligned_test() {
  [<<-1:32>>, <<0:1>>, <<0:0>>]
  |> bit_array.concat
  |> should.equal(<<255, 255, 255, 255, 0:1>>)

  [<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>]
  |> bit_array.concat
  |> should.equal(<<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>)

  [<<1, 2:4>>, <<3>>]
  |> bit_array.concat
  |> should.equal(<<1, 2:4, 3>>)

  [<<-1:32>>, <<0:1>>, <<0:0>>]
  |> bit_array.concat
  |> should.equal(<<255, 255, 255, 255, 0:1>>)

  [<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>]
  |> bit_array.concat
  |> should.equal(<<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>)
}

pub fn slice_unaligned_test() {
  <<0, 1, 2:7>>
  |> bit_array.slice(0, 3)
  |> should.equal(Error(Nil))

  <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>
  |> bit_array.slice(8, 12)
  |> should.equal(Error(Nil))
}

pub fn to_string_unaligned_test() {
  <<"ø":utf8, 50:4>>
  |> bit_array.to_string
  |> should.equal(Error(Nil))

  let assert <<_:3, x:bits>> = <<0:3, "ø":utf8>>
  x
  |> bit_array.to_string
  |> should.equal(Ok("ø"))
}

pub fn base64_unaligned_encode_test() {
  <<-1:7>>
  |> bit_array.base64_encode(True)
  |> should.equal("/g==")

  <<0xFA, 5:3>>
  |> bit_array.base64_encode(True)
  |> should.equal("+qA=")

  <<0xFA, 0xBC, 0x6D, 1:1>>
  |> bit_array.base64_encode(True)
  |> should.equal("+rxtgA==")
}

pub fn base16_encode_unaligned_test() {
  <<-1:7>>
  |> bit_array.base16_encode()
  |> should.equal("FE")

  <<0xFA, 5:3>>
  |> bit_array.base16_encode()
  |> should.equal("FAA0")

  <<0xFA, 5:4>>
  |> bit_array.base16_encode()
  |> should.equal("FA50")

  <<0xFA, 0xBC, 0x6D, 1:1>>
  |> bit_array.base16_encode()
  |> should.equal("FABC6D80")
}

pub fn inspect_unaligned_test() {
  bit_array.inspect(<<4:5>>)
  |> should.equal("<<4:size(5)>>")

  bit_array.inspect(<<100, 5:3>>)
  |> should.equal("<<100, 5:size(3)>>")

  bit_array.inspect(<<5:3, 11:4, 1:2>>)
  |> should.equal("<<182, 1:size(1)>>")
}

pub fn compare_test() {
  bit_array.compare(<<4:5>>, <<4:5>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<4:5, 3:3>>, <<4:5, 2:3>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<4:5, 3:3>>, <<4:5, 4:3>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<4:5, 3:3, 0:0>>, <<4:5, 3:3, 0:0>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<4:2, 3:4, 0:0>>, <<4:2, 3:3, 0:0>>)
  |> should.equal(order.Gt)

  // first is: <<33, 1:size(1)>>
  // second is: <<35>>
  bit_array.compare(<<4:5, 3:4, 0:0>>, <<4:5, 3:3, 0:0>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<3:5>>, <<4:5>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<3:7>>, <<4:7>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<5:5>>, <<4:5>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<4:8>>, <<4:5>>)
  |> should.equal(order.Gt)

  bit_array.compare(<<4:5>>, <<4:8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<0:5>>, <<0:8>>)
  |> should.equal(order.Lt)

  bit_array.compare(<<0:5>>, <<0:5>>)
  |> should.equal(order.Eq)

  bit_array.compare(<<0:2>>, <<0:1>>)
  |> should.equal(order.Gt)
}

pub fn starts_with_unaligned_test() {
  bit_array.starts_with(<<1:1>>, <<1:1>>)
  |> should.be_true

  bit_array.starts_with(<<1:1>>, <<>>)
  |> should.be_true

  bit_array.starts_with(<<1:1>>, <<1:2>>)
  |> should.be_false

  bit_array.starts_with(<<-1:127>>, <<-1:33>>)
  |> should.be_true

  bit_array.starts_with(<<-1:127>>, <<-1:128>>)
  |> should.be_false

  bit_array.starts_with(<<0:127>>, <<1:127>>)
  |> should.be_false

  bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 1:1>>)
  |> should.be_true

  bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 0:1>>)
  |> should.be_false
}

import gleam/bit_string
import gleam/should

pub fn length_test() {
  bit_string.byte_size(bit_string.from_string("hello"))
  |> should.equal(5)

  bit_string.byte_size(bit_string.from_string(""))
  |> should.equal(0)
}

pub fn append_test() {
  bit_string.from_string("Test")
  |> bit_string.append(bit_string.from_string(" Me"))
  |> should.equal(bit_string.from_string("Test Me"))

  let Ok(zero_32bit) = bit_string.int_to_u32(0)
  zero_32bit
  |> bit_string.append(bit_string.from_string(""))
  |> should.equal(zero_32bit)
}

pub fn part_test() {
  bit_string.from_string("hello")
  |> bit_string.part(0, 5)
  |> should.equal(Ok(bit_string.from_string("hello")))

  bit_string.from_string("hello")
  |> bit_string.part(0, 0)
  |> should.equal(Ok(bit_string.from_string("")))

  bit_string.from_string("hello")
  |> bit_string.part(2, 2)
  |> should.equal(Ok(bit_string.from_string("ll")))

  bit_string.from_string("hello")
  |> bit_string.part(5, -2)
  |> should.equal(Ok(bit_string.from_string("lo")))

  bit_string.from_string("")
  |> bit_string.part(0, 0)
  |> should.equal(Ok(bit_string.from_string("")))

  bit_string.from_string("hello")
  |> bit_string.part(6, 0)
  |> should.equal(Error(Nil))

  bit_string.from_string("hello")
  |> bit_string.part(-1, 1)
  |> should.equal(Error(Nil))

  bit_string.from_string("hello")
  |> bit_string.part(1, 6)
  |> should.equal(Error(Nil))
}

pub fn u32_test() {
  let Ok(bin) = bit_string.int_to_u32(0)
  should.equal(4, bit_string.byte_size(bin))
  should.equal(Ok(0), bit_string.int_from_u32(bin))

  let Ok(bin) = bit_string.int_to_u32(4294967295)
  should.equal(4, bit_string.byte_size(bin))
  should.equal(Ok(4294967295), bit_string.int_from_u32(bin))

  should.equal(Error(Nil), bit_string.int_from_u32(bit_string.from_string("")))
  should.equal(
    Error(Nil),
    bit_string.int_from_u32(bit_string.from_string("12345")),
  )
}

pub fn to_string_test() {
  <<>>
  |> bit_string.to_string
  |> should.equal(Ok(""))

  <<"":utf8>>
  |> bit_string.to_string
  |> should.equal(Ok(""))

  <<"Hello":utf8>>
  |> bit_string.to_string
  |> should.equal(Ok("Hello"))

  <<"ø":utf8>>
  |> bit_string.to_string
  |> should.equal(Ok("ø"))

  <<65535:16>>
  |> bit_string.to_string
  |> should.equal(Error(Nil))
}

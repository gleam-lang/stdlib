import gleam/binary
import gleam/should

pub fn length_test() {
  binary.byte_size(binary.from_string("hello"))
  |> should.equal(5)

  binary.byte_size(binary.from_string(""))
  |> should.equal(0)
}

pub fn append_test() {
  binary.from_string("Test")
  |> binary.append(binary.from_string(" Me"))
  |> should.equal(binary.from_string("Test Me"))

  let Ok(zero_32bit) = binary.int_to_u32(0)
  zero_32bit
  |> binary.append(binary.from_string(""))
  |> should.equal(zero_32bit)
}

pub fn part_test() {
  binary.from_string("hello")
  |> binary.part(0, 5)
  |> should.equal(Ok(binary.from_string("hello")))

  binary.from_string("hello")
  |> binary.part(0, 0)
  |> should.equal(Ok(binary.from_string("")))

  binary.from_string("hello")
  |> binary.part(2, 2)
  |> should.equal(Ok(binary.from_string("ll")))

  binary.from_string("hello")
  |> binary.part(5, -2)
  |> should.equal(Ok(binary.from_string("lo")))

  binary.from_string("")
  |> binary.part(0, 0)
  |> should.equal(Ok(binary.from_string("")))

  binary.from_string("hello")
  |> binary.part(6, 0)
  |> should.equal(Error(Nil))

  binary.from_string("hello")
  |> binary.part(-1, 1)
  |> should.equal(Error(Nil))

  binary.from_string("hello")
  |> binary.part(1, 6)
  |> should.equal(Error(Nil))
}

pub fn u32_test() {
  let Ok(bin) = binary.int_to_u32(0)
  should.equal(4, binary.byte_size(bin))
  should.equal(Ok(0), binary.int_from_u32(bin))

  let Ok(bin) = binary.int_to_u32(4294967295)
  should.equal(4, binary.byte_size(bin))
  should.equal(Ok(4294967295), binary.int_from_u32(bin))

  should.equal(Error(Nil), binary.int_from_u32(binary.from_string("")))
  should.equal(Error(Nil), binary.int_from_u32(binary.from_string("12345")))
}

import gleam/base
import gleam/bit_string.{BitString}
import gleam/io
import gleam/list
import gleam/should

external fn list_to_binary(List(Int)) -> BitString =
  "erlang" "list_to_binary"

pub fn encode64_test() {
  [255, 127, 254, 252]
  |> list_to_binary()
  |> base.encode64(True)
  |> should.equal("/3/+/A==")

  [255, 127, 254, 252]
  |> list_to_binary()
  |> base.encode64(False)
  |> should.equal("/3/+/A")

  [0, 0, 0]
  |> list_to_binary()
  |> base.encode64(True)
  |> should.equal("AAAA")

  []
  |> list_to_binary()
  |> base.encode64(True)
  |> should.equal("")
}

pub fn decode64_test() {
  "/3/+/A=="
  |> base.decode64()
  |> should.equal(Ok(list_to_binary([255, 127, 254, 252])))

  "/3/+/A"
  |> base.decode64()
  |> should.equal(Ok(list_to_binary([255, 127, 254, 252])))

  "AAAA"
  |> base.decode64()
  |> should.equal(Ok(list_to_binary([0, 0, 0])))

  ""
  |> base.decode64()
  |> should.equal(Ok(list_to_binary([])))

  ")!"
  |> base.decode64()
  |> should.equal(Error(Nil))
}

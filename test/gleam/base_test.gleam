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

pub fn url_encode64_test() {
  [255, 127, 254, 252]
  |> list_to_binary()
  |> base.url_encode64(True)
  |> should.equal("_3_-_A==")

  [255, 127, 254, 252]
  |> list_to_binary()
  |> base.url_encode64(False)
  |> should.equal("_3_-_A")

  [0, 0, 0]
  |> list_to_binary()
  |> base.url_encode64(True)
  |> should.equal("AAAA")

  []
  |> list_to_binary()
  |> base.url_encode64(True)
  |> should.equal("")
}

pub fn url_decode64_test() {
  "_3_-_A=="
  |> base.url_decode64()
  |> should.equal(Ok(list_to_binary([255, 127, 254, 252])))

  "_3_-_A"
  |> base.url_decode64()
  |> should.equal(Ok(list_to_binary([255, 127, 254, 252])))

  "AAAA"
  |> base.url_decode64()
  |> should.equal(Ok(list_to_binary([0, 0, 0])))

  ""
  |> base.url_decode64()
  |> should.equal(Ok(list_to_binary([])))

  ")!"
  |> base.url_decode64()
  |> should.equal(Error(Nil))
}

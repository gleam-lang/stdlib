import gleam/should
import gleam/iodata

pub fn iodata_test() {
  let data = iodata.new("ello")
    |> iodata.append(",")
    |> iodata.append(" world!")
    |> iodata.prepend("H")

  data
  |> iodata.to_string
  |> should.equal("Hello, world!")

  data
  |> iodata.byte_size
  |> should.equal(13)

  let data = iodata.new("ello")
    |> iodata.append_iodata(iodata.new(","))
    |> iodata.append_iodata(
      iodata.concat([iodata.new(" wo"), iodata.new("rld!")]),
    )
    |> iodata.prepend_iodata(iodata.new("H"))

  data
  |> iodata.to_string
  |> should.equal("Hello, world!")

  data
  |> iodata.byte_size
  |> should.equal(13)
}

pub fn lowercase_test() {
  ["Gleam", "Gleam"]
  |> iodata.from_strings
  |> iodata.lowercase
  |> iodata.to_string
  |> should.equal("gleamgleam")
}

pub fn uppercase_test() {
  ["Gleam", "Gleam"]
  |> iodata.from_strings
  |> iodata.uppercase
  |> iodata.to_string
  |> should.equal("GLEAMGLEAM")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> iodata.new
  |> iodata.split(",")
  |> should.equal(
    [iodata.new("Gleam"), iodata.new("Erlang"), iodata.new("Elixir")],
  )

  ["Gleam, Erl", "ang,Elixir"]
  |> iodata.from_strings
  |> iodata.split(", ")
  |> should.equal(
    [iodata.new("Gleam"), iodata.from_strings(["Erl", "ang,Elixir"])],
  )
}

pub fn is_equal_test() {
  iodata.new("12")
  |> iodata.is_equal(iodata.from_strings(["1", "2"]))
  |> should.be_true

  iodata.new("12")
  |> iodata.is_equal(iodata.new("12"))
  |> should.be_true

  iodata.new("12")
  |> iodata.is_equal(iodata.new("2"))
  |> should.be_false
}

pub fn is_empty_test() {
  iodata.new("")
  |> iodata.is_empty
  |> should.be_true

  iodata.new("12")
  |> iodata.is_empty
  |> should.be_false

  iodata.from_strings([])
  |> iodata.is_empty
  |> should.be_true

  iodata.from_strings(["", ""])
  |> iodata.is_empty
  |> should.be_true
}

import gleam/generic
import gleam/pair
import gleam/should

pub fn identity_test() {
  1
  |> generic.identity
  |> should.equal(1)

  ""
  |> generic.identity
  |> should.equal("")

  []
  |> generic.identity
  |> should.equal([])

  #(1, 2.0)
  |> generic.identity
  |> should.equal(#(1, 2.0))
}

pub fn constant_test() {
  #(1, 2)
  |> pair.map_first(generic.constant(42))
  |> should.equal(#(42, 2))
}

pub fn equal_test() {
  generic.equal(True, False)
  |> should.equal(False)

  generic.equal(False, False)
  |> should.equal(True)

  generic.equal(1, 1)
  |> should.equal(True)

  "Erlang"
  |> generic.equal(to: "Java")
  |> should.equal(False)
}

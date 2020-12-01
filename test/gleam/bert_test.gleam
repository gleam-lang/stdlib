import gleam/bert
import gleam/dynamic
import gleam/should

pub fn term_to_binary_test() {
  let term = tuple("binary", [1, 2, 3])
  term
  |> bert.term_to_binary()
  |> bert.binary_to_term()
  |> should.equal(Ok(dynamic.from(term)))

  term
  |> bert.term_to_binary()
  |> bert.unsafe_binary_to_term()
  |> should.equal(Ok(dynamic.from(term)))

  <<>>
  |> bert.binary_to_term()
  |> should.equal(Error(Nil))
}

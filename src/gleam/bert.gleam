import gleam/dynamic.{Dynamic}
import gleam/function.{rescue}

pub external fn term_to_binary(a) -> BitString =
  "erlang" "term_to_binary"

type Safe {
  Safe
}

external fn erl_binary_to_term(BitString, List(Safe)) -> Dynamic =
  "erlang" "binary_to_term"

pub fn binary_to_term(binary) {
  case rescue(fn() { erl_binary_to_term(binary, [Safe]) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

pub fn unsafe_binary_to_term(binary) {
  case rescue(fn() { erl_binary_to_term(binary, []) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

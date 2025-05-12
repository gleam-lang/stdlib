import gleam/dynamic
import gleam/should

pub fn classify_true_test() {
  dynamic.bool(True)
  |> dynamic.classify
  |> should.equal("Bool")
}

pub fn classify_false_test() {
  dynamic.bool(False)
  |> dynamic.classify
  |> should.equal("Bool")
}

pub fn null_test() {
  dynamic.null()
  |> dynamic.classify
  |> should.equal("Nil")
}

import gleam/dynamic
import gleam/should

pub fn classify_true_test() {
  dynamic.from(True)
  |> dynamic.classify
  |> should.equal("Bool")
}

pub fn classify_false_test() {
  dynamic.from(False)
  |> dynamic.classify
  |> should.equal("Bool")
}

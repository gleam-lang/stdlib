import gleam/dynamic
import gleam/should

pub fn classify_test() {
  dynamic.from(True)
  |> dynamic.classify
  |> should.equal("Bool")

  dynamic.from(False)
  |> dynamic.classify
  |> should.equal("Bool")
}

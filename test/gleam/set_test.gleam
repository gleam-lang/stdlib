import gleam/should
import gleam/set

pub fn size_test() {
  set.new()
  |> set.size
  |> should.equal(0)

  set.new()
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)

  set.new()
  |> set.insert(1)
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)
}

pub fn contains_test() {
  set.new()
  |> set.insert(1)
  |> set.contains(this: 1)
  |> should.be_true

  set.new()
  |> set.contains(this: 1)
  |> should.be_false
}

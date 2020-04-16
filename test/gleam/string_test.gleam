import gleam/string
import gleam/should
import gleam/order

pub fn length_test() {
  string.length("ß↑e̊")
  |> should.equal(_, 3)

  string.length("Gleam")
  |> should.equal(_, 5)

  string.length("")
  |> should.equal(_, 0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> should.equal(_, "gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> should.equal(_, "GLEAM")
}

pub fn reverse_test() {
  string.reverse("Gleam")
  |> should.equal(_, "maelG")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(_, ",")
  |> should.equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(_, ", ")
  |> should.equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(_, ",", "++")
  |> should.equal(_, "Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(_, " Me")
  |> should.equal(_, "Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> should.equal(_, order.Eq)

  string.compare("a", "")
  |> should.equal(_, order.Gt)

  string.compare("a", "A")
  |> should.equal(_, order.Gt)

  string.compare("A", "B")
  |> should.equal(_, order.Lt)

  string.compare("t", "ABC")
  |> should.equal(_, order.Gt)
}

pub fn contains_test() {
    "gleam"
    |> string.contains(_, "ea")
    |> should.equal(_, True)

    "gleam"
    |> string.contains(_, "x")
    |> should.equal(_, False)

    string.contains(does: "bellwether", contain: "bell")
    |> should.equal(_, True)
}

pub fn concat_test() {
  [
    "Hello", ", ", "world!",
  ]
  |> string.concat
  |> should.equal(_, "Hello, world!")
}

pub fn repeat_test() {
  "hi"
  |> string.repeat(_, times: 3)
  |> should.equal(_, "hihihi")

  "hi"
  |> string.repeat(_, 0)
  |> should.equal(_, "")

  "hi"
  |> string.repeat(_, -1)
  |> should.equal(_, "")
}

pub fn join_test() {
  [
    "Hello", "world!",
  ]
  |> string.join(_, with: ", ")
  |> should.equal(_, "Hello, world!")

  [
    "Hello", "world!",
  ]
  |> string.join(_, with: "-")
  |> should.equal(_, "Hello-world!")
}

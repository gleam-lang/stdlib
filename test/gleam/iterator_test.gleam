import gleam/expect
import gleam/iterator

pub fn to_from_list_test() {
  let test = fn(subject) {
    subject
    |> iterator.from_list
    |> iterator.to_list
    |> expect.equal(_, subject)
  }

  test([])
  test([1])
  test([1, 2])
  test([1, 2, 4, 8])
}

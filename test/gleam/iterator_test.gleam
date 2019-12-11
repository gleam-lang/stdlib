import gleam/expect
import gleam/iterator
import gleam/list

// TODO: Property tests

// a |> from_list |> to_list == a
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

// a |> from_list |> take(_, n) == a |> list.take(_, n)
pub fn take_test() {
  let test = fn(n, subject) {
    subject
    |> iterator.from_list
    |> iterator.take(_, n)
    |> expect.equal(_, list.take(subject, n))
  }

  test(0, [])
  test(1, [])
  test(-1, [])
  test(0, [0])
  test(1, [0])
  test(-1, [0])
  test(0, [0, 1, 2, 3, 4])
  test(1, [0, 1, 2, 3, 4])
  test(2, [0, 1, 2, 3, 4])
  test(22, [0, 1, 2, 3, 4])
}

// a |> from_list |> fold(_, a, f) == a |> list.fold(_, a, f)
pub fn fold_test() {
  let test = fn(subject, acc, f) {
    subject
    |> iterator.from_list
    |> iterator.fold(_, acc, f)
    |> expect.equal(_, list.fold(subject, acc, f))
  }

  let f = fn(e, acc) { [e | acc] }
  test([], [], f)
  test([1], [], f)
  test([1, 2, 3], [], f)
  test([1, 2, 3, 4, 5, 6, 7, 8], [], f)
}

// a |> from_list |> map(_, f) |> to_list == a |> list.map(_, f)
pub fn map_test() {
  let test = fn(subject, f) {
    subject
    |> iterator.from_list
    |> iterator.map(_, f)
    |> iterator.to_list
    |> expect.equal(_, list.map(subject, f))
  }

  let f = fn(e) { e * 2 }
  test([], f)
  test([1], f)
  test([1, 2, 3], f)
  test([1, 2, 3, 4, 5, 6, 7, 8], f)
}

// a |> from_list |> filter(_, f) |> to_list == a |> list.filter(_, f)
pub fn filter_test() {
  let test = fn(subject, f) {
    subject
    |> iterator.from_list
    |> iterator.filter(_, f)
    |> iterator.to_list
    |> expect.equal(_, list.filter(subject, f))
  }

  let even = fn(x) { x % 2 == 0 }
  test([], even)
  test([1], even)
  test([1, 2], even)
  test([1, 2, 3], even)
  test([1, 2, 3, 4], even)
  test([1, 2, 3, 4, 5], even)
  test([1, 2, 3, 4, 5, 6], even)
}

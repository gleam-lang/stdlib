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

pub fn repeat_test() {
  1
  |> iterator.repeat
  |> iterator.take(_, 5)
  |> expect.equal(_, [1, 1, 1, 1, 1])
}

pub fn cycle_test() {
  [1, 2, 3]
  |> iterator.from_list
  |> iterator.cycle
  |> iterator.take(_, 9)
  |> expect.equal(_, [1, 2, 3, 1, 2, 3, 1, 2, 3])
}

pub fn unfold_test() {
  iterator.unfold(2, fn(acc) { iterator.Next(acc, acc * 2) })
  |> iterator.take(_, 5)
  |> expect.equal(_, [2, 4, 8, 16, 32])

  iterator.unfold(2, fn(_) { iterator.Done })
  |> iterator.take(_, 5)
  |> expect.equal(_, [])
}

pub fn range_test() {
  let test = fn(a, b, expected) {
    iterator.range(a, b)
    |> iterator.to_list
    |> expect.equal(_, expected)
  }

  test(0, 0, [])
  test(1, 1, [])
  test(-1, -1, [])
  test(0, 1, [0])
  test(0, 5, [0, 1, 2, 3, 4])
  test(1, -5, [1, 0, -1, -2, -3, -4])
}

pub fn drop_test() {
  iterator.range(0, 10)
  |> iterator.drop(_, 5)
  |> iterator.to_list
  |> expect.equal(_, [5, 6, 7, 8, 9])
}

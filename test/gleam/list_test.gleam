import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/pair
import gleam/should
import gleam/string

@target(erlang)
const recursion_test_cycles = 1_000_000

// JavaScript engines crash when exceeding a certain stack size:
//
// - Chrome 106 and NodeJS V16, V18, and V19 crash around 10_000+
// - Firefox 106 crashes around 35_000+.
// - Safari 16 crashes around 40_000+.
@target(javascript)
const recursion_test_cycles = 40_000

pub fn length_test() {
  list.length([])
  |> should.equal(0)

  list.length([1])
  |> should.equal(1)

  list.length([1, 1])
  |> should.equal(2)

  list.length([1, 1, 1])
  |> should.equal(3)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.length()
}

pub fn count_test() {
  list.count([], int.is_odd)
  |> should.equal(0)

  list.count([2, 4, 6], int.is_odd)
  |> should.equal(0)

  list.count([1, 2, 3, 4, 5], int.is_odd)
  |> should.equal(3)

  list.count(["a", "list", "with", "some", "string", "values"], fn(a) {
    string.length(a) > 4
  })
  |> should.equal(2)
}

pub fn reverse_test() {
  list.reverse([])
  |> should.equal([])

  list.reverse([1])
  |> should.equal([1])

  list.reverse([1, 2])
  |> should.equal([2, 1])

  list.reverse([1, 2, 3, 4, 5])
  |> should.equal([5, 4, 3, 2, 1])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.reverse
}

pub fn is_empty_test() {
  list.is_empty([])
  |> should.be_true

  list.is_empty([1])
  |> should.be_false
}

pub fn contains_test() {
  list.contains([0, 4, 5, 1], 1)
  |> should.be_true

  list.contains([0, 4, 5, 7], 1)
  |> should.be_false

  list.contains([], 1)
  |> should.be_false

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.contains(1)
}

pub fn first_test() {
  list.first([0, 4, 5, 7])
  |> should.equal(Ok(0))

  list.first([])
  |> should.equal(Error(Nil))
}

pub fn rest_test() {
  list.rest([0, 4, 5, 7])
  |> should.equal(Ok([4, 5, 7]))

  list.rest([0])
  |> should.equal(Ok([]))

  list.rest([])
  |> should.equal(Error(Nil))
}

pub fn group_test() {
  [Ok(10), Error("Wrong"), Ok(200), Ok(1)]
  |> list.group(fn(i) {
    case i {
      Ok(_) -> "Successful"
      Error(_) -> "Failed"
    }
  })
  |> should.equal(
    dict.new()
    |> dict.insert("Failed", [Error("Wrong")])
    |> dict.insert("Successful", [Ok(1), Ok(200), Ok(10)]),
  )

  list.group([1, 2, 3, 4, 5], fn(i) { i - i / 3 * 3 })
  |> should.equal(
    dict.new()
    |> dict.insert(0, [3])
    |> dict.insert(1, [4, 1])
    |> dict.insert(2, [5, 2]),
  )
}

pub fn filter_test() {
  []
  |> list.filter(fn(_) { True })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(_) { True })
  |> should.equal([0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x > 4 })
  |> should.equal([5, 7])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x < 4 })
  |> should.equal([0, 3])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.filter(fn(x) { x == -1 })
}

pub fn filter_map_test() {
  [2, 4, 6, 1]
  |> list.filter_map(fn(x) { Ok(x + 1) })
  |> should.equal([3, 5, 7, 2])

  [2, 4, 6, 1]
  |> list.filter_map(Error)
  |> should.equal([])

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.filter_map(fn(x) { Ok(x + 1) })
}

pub fn map_test() {
  []
  |> list.map(fn(x) { x * 2 })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.map(fn(x) { x * 2 })
  |> should.equal([0, 8, 10, 14, 6])

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.map(fn(x) { x })
}

pub fn map2_test() {
  list.map2([1, 2, 3], [], int.add)
  |> should.equal([])

  list.map2([], [1, 2, 3], int.add)
  |> should.equal([])

  list.map2([], [], int.add)
  |> should.equal([])

  list.map2([1, 2, 3], [4, 5], int.add)
  |> should.equal([5, 7])

  list.map2([1, 2, 3], [4, 5, 6], int.add)
  |> should.equal([5, 7, 9])

  list.map2([1, 2, 3], ["1", "2"], pair.new)
  |> should.equal([#(1, "1"), #(2, "2")])

  // TCO test
  let list = list.repeat(0, recursion_test_cycles)
  list.map2(list, list, int.add)
}

pub fn map_fold_test() {
  [1, 2, 3, 4]
  |> list.map_fold(from: 0, with: fn(acc, i) { #(acc + i, i * 2) })
  |> should.equal(#(10, [2, 4, 6, 8]))

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.map_fold(from: 0, with: fn(acc, i) { #(acc + i, i + 1) })
}

pub fn try_map_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  [5, 6, 5, 6]
  |> list.try_map(fun)
  |> should.equal(Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
  |> list.try_map(fun)
  |> should.equal(Error(7))

  // TCO test
  list.repeat(6, recursion_test_cycles)
  |> list.try_map(fun)
}

pub fn drop_test() {
  []
  |> list.drop(5)
  |> should.equal([])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.drop(5)
  |> should.equal([6, 7, 8])

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.drop(recursion_test_cycles)
}

pub fn take_test() {
  []
  |> list.take(5)
  |> should.equal([])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.take(5)
  |> should.equal([1, 2, 3, 4, 5])

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.take(recursion_test_cycles)
}

pub fn new_test() {
  list.new()
  |> should.equal([])
}

pub fn wrap_test() {
  list.wrap([])
  |> should.equal([[]])

  list.wrap([[]])
  |> should.equal([[[]]])

  list.wrap(Nil)
  |> should.equal([Nil])

  list.wrap(1)
  |> should.equal([1])

  list.wrap([1, 2])
  |> should.equal([[1, 2]])

  list.wrap([[1, 2, 3]])
  |> should.equal([[[1, 2, 3]]])
}

pub fn append_test() {
  list.append([1], [2, 3])
  |> should.equal([1, 2, 3])

  list.append([1, 2], [])
  |> should.equal([1, 2])

  list.append([], [1, 2])
  |> should.equal([1, 2])

  list.append([1, 2], [3, 4])
  |> should.equal([1, 2, 3, 4])

  list.append([1, 2, 3], [])
  |> should.equal([1, 2, 3])

  list.append([1, 2, 3], [4])
  |> should.equal([1, 2, 3, 4])

  list.append([1, 2, 3, 4], [5])
  |> should.equal([1, 2, 3, 4, 5])

  list.append([], [])
  |> should.equal([])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.append([1])
}

pub fn concat_test() {
  list.concat([])
  |> should.equal([])

  list.concat([[]])
  |> should.equal([])

  list.concat([[], [], []])
  |> should.equal([])

  list.concat([[1, 2], [], [3, 4]])
  |> should.equal([1, 2, 3, 4])
  // // TCO test
  // case recursion_test_cycles > 2 {
  //   True ->
  //     list.repeat([[1]], recursion_test_cycles / 50)
  //     |> list.concat()
  //   False -> []
  // }
}

pub fn flatten_test() {
  list.flatten([])
  |> should.equal([])

  list.flatten([[]])
  |> should.equal([])

  list.flatten([[], [], []])
  |> should.equal([])

  list.flatten([[1, 2], [], [3, 4]])
  |> should.equal([1, 2, 3, 4])
}

pub fn flat_map_test() {
  list.flat_map([1, 10, 20], fn(x) { [x, x + 1] })
  |> should.equal([1, 2, 10, 11, 20, 21])
}

pub fn fold_test() {
  [1, 2, 3]
  |> list.fold([], fn(acc, x) { [x, ..acc] })
  |> should.equal([3, 2, 1])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.fold([], fn(acc, x) { [x, ..acc] })
}

pub fn fold_right_test() {
  [1, 2, 3]
  |> list.fold_right(from: [], with: fn(acc, x) { [x, ..acc] })
  |> should.equal([1, 2, 3])
}

pub fn index_fold_test() {
  ["a", "b", "c"]
  |> list.index_fold([], fn(acc, i, ix) { [#(ix, i), ..acc] })
  |> should.equal([#(2, "c"), #(1, "b"), #(0, "a")])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.index_fold([], fn(acc, i, ix) { [#(ix, i), ..acc] })
}

pub fn fold_until_test() {
  [1, 2, 3, 4]
  |> list.fold_until(from: 0, with: fn(acc, n) {
    case n < 4 {
      True -> list.Continue(acc + n)
      False -> list.Stop(acc)
    }
  })
  |> should.equal(6)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.fold_until(from: 0, with: fn(acc, n) {
    case n < recursion_test_cycles {
      True -> list.Continue(acc + n)
      False -> list.Stop(acc)
    }
  })
}

pub fn try_fold_test() {
  [1, 2, 3]
  |> list.try_fold(0, fn(acc, i) {
    case i < 4 {
      True -> Ok(acc + i)
      False -> Error(Nil)
    }
  })
  |> should.equal(Ok(6))

  [1, 2, 3]
  |> list.try_fold(0, fn(acc, i) {
    case i < 3 {
      True -> Ok(acc + i)
      False -> Error(Nil)
    }
  })
  |> should.equal(Error(Nil))

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.try_fold(0, fn(acc, i) {
    case i < recursion_test_cycles {
      True -> Ok(acc + i)
      False -> Error(Nil)
    }
  })
}

pub fn find_map_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  [1, 2, 3]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3, 2]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3]
  |> list.find_map(with: f)
  |> should.equal(Error(Nil))

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.find_map(with: fn(x) {
    case x == recursion_test_cycles {
      True -> Ok(recursion_test_cycles)
      _ -> Error(Nil)
    }
  })
}

pub fn find_test() {
  let is_two = fn(x) { x == 2 }

  [1, 2, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3, 2]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Error(Nil))

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.find(one_that: fn(x) { x == recursion_test_cycles })
}

pub fn all_test() {
  list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })
  |> should.be_true

  list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.be_false

  list.all([], fn(_) { False })
  |> should.be_true

  [1, 2, 3]
  |> list.all(fn(x) {
    case x {
      1 -> True
      2 -> False
      // Crash if no short-circuit
      _ -> panic
    }
  })

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.all(fn(x) {
    case x {
      0 -> True
      _ -> False
    }
  })
}

pub fn any_test() {
  list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })
  |> should.be_true

  list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.be_false

  list.any([], fn(_) { False })
  |> should.be_false

  [1, 2, 3]
  |> list.any(fn(x) {
    case x {
      1 -> False
      2 -> True
      // Crash if no short-circuit
      _ -> panic
    }
  })

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.any(fn(x) { x == recursion_test_cycles })
}

pub fn zip_test() {
  list.zip([], [1, 2, 3])
  |> should.equal([])

  list.zip([1, 2], [])
  |> should.equal([])

  list.zip([1, 2, 3], [4, 5, 6])
  |> should.equal([#(1, 4), #(2, 5), #(3, 6)])

  list.zip([5, 6], [1, 2, 3])
  |> should.equal([#(5, 1), #(6, 2)])

  list.zip([5, 6, 7], [1, 2])
  |> should.equal([#(5, 1), #(6, 2)])

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
}

pub fn strict_zip_test() {
  list.strict_zip([], [1, 2, 3])
  |> should.equal(Error(Nil))

  list.strict_zip([1, 2], [])
  |> should.equal(Error(Nil))

  list.strict_zip([1, 2, 3], [4, 5, 6])
  |> should.equal(Ok([#(1, 4), #(2, 5), #(3, 6)]))

  list.strict_zip([5, 6], [1, 2, 3])
  |> should.equal(Error(Nil))

  list.strict_zip([5, 6, 7], [1, 2])
  |> should.equal(Error(Nil))
}

pub fn unzip_test() {
  list.unzip([#(1, 2), #(3, 4)])
  |> should.equal(#([1, 3], [2, 4]))

  list.unzip([])
  |> should.equal(#([], []))

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.unzip()
}

pub fn intersperse_test() {
  list.intersperse([1, 2, 3], 4)
  |> should.equal([1, 4, 2, 4, 3])

  list.intersperse([], 2)
  |> should.equal([])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.intersperse(0)
}

pub fn unique_test() {
  list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6])
  |> should.equal([1, 2, 3, 4, 5, 6])

  list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5])
  |> should.equal([7, 1, 45, 6, 2, 47, 5])

  list.unique([3, 4, 5])
  |> should.equal([3, 4, 5])

  list.unique([])
  |> should.equal([])

  // TCO test
  list.range(0, recursion_test_cycles / 100)
  |> list.unique()
}

pub fn sort_test() {
  []
  |> list.sort(int.compare)
  |> should.equal([])

  [1]
  |> list.sort(int.compare)
  |> should.equal([1])

  [2, 1]
  |> list.sort(int.compare)
  |> should.equal([1, 2])

  [3, 1, 2]
  |> list.sort(int.compare)
  |> should.equal([1, 2, 3])

  [4, 3, 6, 5, 4]
  |> list.sort(int.compare)
  |> should.equal([3, 4, 4, 5, 6])

  [4, 3, 6, 5, 4, 1]
  |> list.sort(int.compare)
  |> should.equal([1, 3, 4, 4, 5, 6])

  [4.1, 3.1, 6.1, 5.1, 4.1]
  |> list.sort(float.compare)
  |> should.equal([3.1, 4.1, 4.1, 5.1, 6.1])

  []
  |> list.sort(int.compare)
  |> should.equal([])

  // Stability tests
  let sorted_cards = [
    #(1, 1),
    #(2, 1),
    #(3, 1),
    #(4, 1),
    #(1, 2),
    #(2, 2),
    #(3, 2),
    #(4, 2),
    #(1, 3),
    #(2, 3),
    #(3, 3),
    #(4, 3),
    #(1, 4),
    #(2, 4),
    #(3, 4),
    #(4, 4),
  ]
  let shuffled_cards = [
    #(3, 2),
    #(1, 4),
    #(2, 1),
    #(3, 3),
    #(4, 1),
    #(3, 4),
    #(1, 2),
    #(4, 4),
    #(3, 1),
    #(1, 1),
    #(2, 2),
    #(2, 4),
    #(4, 2),
    #(4, 3),
    #(1, 3),
    #(2, 3),
  ]
  shuffled_cards
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
  |> should.equal(sorted_cards)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.reverse
  |> list.sort(int.compare)
}

pub fn index_map_test() {
  list.index_map([3, 4, 5], fn(x, i) { #(i, x) })
  |> should.equal([#(0, 3), #(1, 4), #(2, 5)])

  let f = fn(x, i) { #(x, i) }
  list.index_map(["a", "b"], f)
  |> should.equal([#("a", 0), #("b", 1)])

  let f = fn(x, i) { #(x, i) }
  list.index_map(["a", "b", "c"], f)
  |> should.equal([#("a", 0), #("b", 1), #("c", 2)])
}

pub fn range_test() {
  list.range(0, 0)
  |> should.equal([0])

  list.range(1, 1)
  |> should.equal([1])

  list.range(-1, -1)
  |> should.equal([-1])

  list.range(0, 1)
  |> should.equal([0, 1])

  list.range(0, 5)
  |> should.equal([0, 1, 2, 3, 4, 5])

  list.range(1, -5)
  |> should.equal([1, 0, -1, -2, -3, -4, -5])

  // TCO test
  list.range(1, recursion_test_cycles)
}

pub fn repeat_test() {
  list.repeat(1, -10)
  |> should.equal([])

  list.repeat(1, 0)
  |> should.equal([])

  list.repeat(2, 3)
  |> should.equal([2, 2, 2])

  list.repeat("x", 5)
  |> should.equal(["x", "x", "x", "x", "x"])

  // TCO test
  list.repeat(0, recursion_test_cycles)
}

pub fn split_test() {
  []
  |> list.split(0)
  |> should.equal(#([], []))

  [0, 1, 2, 3, 4]
  |> list.split(0)
  |> should.equal(#([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(-2)
  |> should.equal(#([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(1)
  |> should.equal(#([0], [1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(3)
  |> should.equal(#([0, 1, 2], [3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(9)
  |> should.equal(#([0, 1, 2, 3, 4], []))

  // TCO test
  list.repeat(0, recursion_test_cycles + 10)
  |> list.split(recursion_test_cycles + 1)
}

pub fn split_while_test() {
  []
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(#([], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(#([1, 2, 3, 4, 5], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x == 2 })
  |> should.equal(#([], [1, 2, 3, 4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 3 })
  |> should.equal(#([1, 2, 3], [4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= -3 })
  |> should.equal(#([], [1, 2, 3, 4, 5]))

  // TCO test
  list.repeat(0, recursion_test_cycles + 10)
  |> list.split_while(fn(x) { x <= recursion_test_cycles + 1 })
}

pub fn key_find_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  proplist
  |> list.key_find(0)
  |> should.equal(Ok("1"))

  proplist
  |> list.key_find(1)
  |> should.equal(Ok("2"))

  proplist
  |> list.key_find(2)
  |> should.equal(Error(Nil))
}

pub fn key_filter_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  proplist
  |> list.key_filter(0)
  |> should.equal(["1", "3"])

  proplist
  |> list.key_filter(1)
  |> should.equal(["2", "4"])

  proplist
  |> list.key_filter(2)
  |> should.equal(["5"])

  proplist
  |> list.key_filter(3)
  |> should.equal([])
}

pub fn pop_test() {
  [1, 2, 3]
  |> list.pop(fn(x) { x > 2 })
  |> should.equal(Ok(#(3, [1, 2])))

  [1, 2, 3]
  |> list.pop(fn(x) { x > 4 })
  |> should.equal(Error(Nil))

  []
  |> list.pop(fn(_x) { True })
  |> should.equal(Error(Nil))

  // TCO test
  list.repeat(0, recursion_test_cycles + 10)
  |> list.pop(fn(x) { x > recursion_test_cycles + 1 })
}

pub fn pop_map_test() {
  let get = fn(x) {
    case x > 0 {
      True -> Ok(x * 2)
      False -> Error(Nil)
    }
  }
  list.pop_map([0, 2, 3], get)
  |> should.equal(Ok(#(4, [0, 3])))

  list.pop_map([0, -1], get)
  |> should.equal(Error(Nil))

  list.pop_map([], get)
  |> should.equal(Error(Nil))
}

pub fn key_pop_test() {
  list.key_pop([#("a", 0), #("b", 1)], "a")
  |> should.equal(Ok(#(0, [#("b", 1)])))

  list.key_pop([#("a", 0), #("b", 1)], "b")
  |> should.equal(Ok(#(1, [#("a", 0)])))

  list.key_pop([#("a", 0), #("b", 1)], "c")
  |> should.equal(Error(Nil))
}

pub fn key_set_test() {
  [#(5, 0), #(4, 1)]
  |> list.key_set(4, 100)
  |> should.equal([#(5, 0), #(4, 100)])

  [#(5, 0), #(4, 1)]
  |> list.key_set(1, 100)
  |> should.equal([#(5, 0), #(4, 1), #(1, 100)])

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.key_set(0, 0)
}

pub fn each_test() {
  list.each([1, 1, 1], fn(x) {
    let assert 1 = x
  })
  |> should.equal(Nil)

  // TCO test
  list.each(list.repeat(1, recursion_test_cycles), fn(x) {
    let assert 1 = x
  })
}

pub fn try_each_test() {
  let assert Ok(Nil) =
    list.try_each(over: [1, 1, 1], with: fn(x) {
      should.equal(x, 1)
      Ok(Nil)
    })

  // `try_each` actually stops when `fun` returns error
  let assert Error(1) =
    list.try_each(over: [1, 2, 3], with: fn(x) {
      should.equal(x, 1)
      Error(x)
    })

  // TCO test
  let assert Ok(Nil) =
    list.repeat(1, recursion_test_cycles)
    |> list.try_each(with: fn(_) { Ok(Nil) })
}

pub fn partition_test() {
  [1, 2, 3, 4, 5, 6, 7]
  |> list.partition(int.is_odd)
  |> should.equal(#([1, 3, 5, 7], [2, 4, 6]))

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.partition(int.is_even)
}

pub fn permutations_test() {
  [1, 2]
  |> list.permutations
  |> should.equal([[1, 2], [2, 1]])

  [1, 2, 3]
  |> list.permutations
  |> should.equal([
    [1, 2, 3],
    [1, 3, 2],
    [2, 1, 3],
    [2, 3, 1],
    [3, 1, 2],
    [3, 2, 1],
  ])

  [1, 2, 3, 4]
  |> list.permutations
  |> should.equal([
    [1, 2, 3, 4],
    [1, 2, 4, 3],
    [1, 3, 2, 4],
    [1, 3, 4, 2],
    [1, 4, 2, 3],
    [1, 4, 3, 2],
    [2, 1, 3, 4],
    [2, 1, 4, 3],
    [2, 3, 1, 4],
    [2, 3, 4, 1],
    [2, 4, 1, 3],
    [2, 4, 3, 1],
    [3, 1, 2, 4],
    [3, 1, 4, 2],
    [3, 2, 1, 4],
    [3, 2, 4, 1],
    [3, 4, 1, 2],
    [3, 4, 2, 1],
    [4, 1, 2, 3],
    [4, 1, 3, 2],
    [4, 2, 1, 3],
    [4, 2, 3, 1],
    [4, 3, 1, 2],
    [4, 3, 2, 1],
  ])

  ["a", "b"]
  |> list.permutations
  |> should.equal([["a", "b"], ["b", "a"]])

  [1, 1]
  |> list.permutations
  |> should.equal([[1, 1], [1, 1]])

  [1, 1, 1]
  |> list.permutations
  |> should.equal([
    [1, 1, 1],
    [1, 1, 1],
    [1, 1, 1],
    [1, 1, 1],
    [1, 1, 1],
    [1, 1, 1],
  ])

  [1, 2, 2]
  |> list.permutations
  |> should.equal([
    [1, 2, 2],
    [1, 2, 2],
    [2, 1, 2],
    [2, 2, 1],
    [2, 1, 2],
    [2, 2, 1],
  ])

  ["a", "a", "a", "a"]
  |> list.permutations
  |> should.equal([
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
    ["a", "a", "a", "a"],
  ])
}

pub fn window_test() {
  [1, 2, 3]
  |> list.window(by: 2)
  |> should.equal([[1, 2], [2, 3]])

  [1, 2, 3]
  |> list.window(3)
  |> should.equal([[1, 2, 3]])

  [1, 2, 3]
  |> list.window(4)
  |> should.equal([])

  [1, 2, 3, 4, 5]
  |> list.window(3)
  |> should.equal([[1, 2, 3], [2, 3, 4], [3, 4, 5]])

  [1, 2, 3]
  |> list.window(0)
  |> should.equal([])

  [1, 2, 3]
  |> list.window(-1)
  |> should.equal([])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.window(2)
}

pub fn window_by_2_test() {
  [1, 2, 3, 4]
  |> list.window_by_2
  |> should.equal([#(1, 2), #(2, 3), #(3, 4)])

  [1]
  |> list.window_by_2
  |> should.equal([])
}

pub fn drop_while_test() {
  [1, 2, 3, 4]
  |> list.drop_while(fn(x) { x < 3 })
  |> should.equal([3, 4])

  // TCO test
  list.range(0, recursion_test_cycles + 10)
  |> list.drop_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn take_while_test() {
  [1, 2, 3, 2, 4]
  |> list.take_while(fn(x) { x < 3 })
  |> should.equal([1, 2])

  // TCO test
  list.range(0, recursion_test_cycles + 10)
  |> list.take_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn chunk_test() {
  [1, 2, 3]
  |> list.chunk(by: fn(n) { n % 2 })
  |> should.equal([[1], [2], [3]])

  [1, 2, 2, 3, 4, 4, 6, 7, 7]
  |> list.chunk(by: fn(n) { n % 2 })
  |> should.equal([[1], [2, 2], [3], [4, 4, 6], [7, 7]])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.chunk(by: fn(n) { n % 2 })
}

pub fn sized_chunk_test() {
  [1, 2, 3, 4, 5, 6]
  |> list.sized_chunk(into: 2)
  |> should.equal([[1, 2], [3, 4], [5, 6]])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.sized_chunk(into: 3)
  |> should.equal([[1, 2, 3], [4, 5, 6], [7, 8]])

  // TCO test
  list.range(0, recursion_test_cycles * 3)
  |> list.sized_chunk(into: 3)
}

pub fn reduce_test() {
  []
  |> list.reduce(with: fn(x, y) { x + y })
  |> should.equal(Error(Nil))

  [1, 2, 3, 4, 5]
  |> list.reduce(with: fn(x, y) { x + y })
  |> should.equal(Ok(15))
}

pub fn scan_test() {
  []
  |> list.scan(from: 0, with: fn(acc, i) { i + acc })
  |> should.equal([])

  [1, 2, 3, 4]
  |> list.scan(from: 0, with: fn(acc, i) { 2 * i + acc })
  |> should.equal([2, 6, 12, 20])

  [1, 2, 3, 4]
  |> list.scan(from: [], with: fn(acc, i) {
    case int.is_even(i) {
      True -> ["Even", ..acc]
      False -> ["Odd", ..acc]
    }
  })
  |> should.equal([
    ["Odd"],
    ["Even", "Odd"],
    ["Odd", "Even", "Odd"],
    ["Even", "Odd", "Even", "Odd"],
  ])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.scan(from: 0, with: fn(acc, i) { i + acc })
}

pub fn last_test() {
  list.last([])
  |> should.equal(Error(Nil))

  list.last([1, 2, 3, 4, 5])
  |> should.equal(Ok(5))
}

pub fn combinations_test() {
  list.combinations([1, 2, 3], by: 0)
  |> should.equal([[]])

  list.combinations([1, 2, 3], by: 1)
  |> should.equal([[1], [2], [3]])

  list.combinations([1, 2, 3], by: 2)
  |> should.equal([[1, 2], [1, 3], [2, 3]])

  list.combinations([1, 2, 3], by: 3)
  |> should.equal([[1, 2, 3]])

  list.combinations([1, 2, 3], by: 4)
  |> should.equal([])

  list.combinations([1, 2, 3, 4], 3)
  |> should.equal([[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]])
}

pub fn combination_pairs_test() {
  list.combination_pairs([1])
  |> should.equal([])

  list.combination_pairs([1, 2])
  |> should.equal([#(1, 2)])

  list.combination_pairs([1, 2, 3])
  |> should.equal([#(1, 2), #(1, 3), #(2, 3)])

  list.combination_pairs([1, 2, 3, 4])
  |> should.equal([#(1, 2), #(1, 3), #(1, 4), #(2, 3), #(2, 4), #(3, 4)])

  // TCO test
  list.range(0, 200)
  |> list.combination_pairs()
}

pub fn interleave_test() {
  list.interleave([[1, 2], [101, 102]])
  |> should.equal([1, 101, 2, 102])

  list.interleave([[1, 2], [101, 102], [201, 202]])
  |> should.equal([1, 101, 201, 2, 102, 202])

  // Left over elements are added at the end
  list.interleave([[1, 2, 3], [101, 102]])
  |> should.equal([1, 101, 2, 102, 3])

  list.interleave([[1, 2], [101, 102, 103]])
  |> should.equal([1, 101, 2, 102, 103])
}

pub fn transpose_test() {
  list.transpose([[1, 2, 3], [101, 102, 103]])
  |> should.equal([[1, 101], [2, 102], [3, 103]])

  list.transpose([[1, 2, 3], [101, 102, 103], [201, 202, 203]])
  |> should.equal([[1, 101, 201], [2, 102, 202], [3, 103, 203]])

  // Left over elements are still returned
  list.transpose([[1, 2], [101, 102, 103]])
  |> should.equal([[1, 101], [2, 102], [103]])

  list.transpose([[1, 2, 3], [101, 102], [201, 202, 203]])
  |> should.equal([[1, 101, 201], [2, 102, 202], [3, 203]])
}

pub fn shuffle_test() {
  []
  |> list.shuffle
  |> should.equal([])

  [1, 1]
  |> list.shuffle
  |> should.equal([1, 1])

  [1, 1, 1]
  |> list.shuffle
  |> should.equal([1, 1, 1])

  list.range(1, 100)
  |> list.shuffle
  |> list.sort(int.compare)
  |> should.equal(list.range(1, 100))

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.shuffle()
}

pub fn max_test() {
  []
  |> list.max(int.compare)
  |> should.equal(Error(Nil))

  [1, 3, 2]
  |> list.max(int.compare)
  |> should.equal(Ok(3))

  [-1.0, 1.2, 1.104]
  |> list.max(float.compare)
  |> should.equal(Ok(1.2))

  ["a", "c", "b"]
  |> list.max(string.compare)
  |> should.equal(Ok("c"))
}

pub fn sample_test() {
  []
  |> list.sample(3)
  |> should.equal([])

  [1, 2, 3]
  |> list.sample(0)
  |> should.equal([])

  [1, 2, 3]
  |> list.sample(-1)
  |> should.equal([])

  [1, 2]
  |> list.sample(5)
  |> list.sort(int.compare)
  |> should.equal([1, 2])

  [1]
  |> list.sample(1)
  |> should.equal([1])

  let input = list.range(1, 100)
  let sample = list.sample(input, 10)
  list.length(sample)
  |> should.equal(10)

  let repeated = [1, 1, 1, 1, 1]
  let sample = list.sample(repeated, 3)
  sample
  |> list.all(fn(x) { x == 1 })
  |> should.be_true()

  let input = list.range(1, 1000)
  let sample = list.sample(input, 100)
  sample
  |> list.sort(int.compare)
  |> list.all(fn(x) { x >= 1 && x <= 1000 })
  |> should.be_true()

  list.length(sample)
  |> should.equal(100)

  let min = list.fold(sample, 1000, int.min)
  let max = list.fold(sample, 1, int.max)
  should.be_true(min >= 1)
  should.be_true(max <= 1000)
}

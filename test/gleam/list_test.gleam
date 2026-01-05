import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/pair
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
  let zero = 0
  assert list.length([]) == zero

  assert list.length([1]) == 1

  assert list.length([1, 1]) == 2

  assert list.length([1, 1, 1]) == 3

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.length()
}

pub fn count_test() {
  assert list.count([], int.is_odd) == 0

  assert list.count([2, 4, 6], int.is_odd) == 0

  assert list.count([1, 2, 3, 4, 5], int.is_odd) == 3

  assert list.count(["a", "list", "with", "some", "string", "values"], fn(a) {
      string.length(a) > 4
    })
    == 2
}

pub fn reverse_test() {
  assert list.reverse([]) == []

  assert list.reverse([1]) == [1]

  assert list.reverse([1, 2]) == [2, 1]

  assert list.reverse([1, 2, 3, 4, 5]) == [5, 4, 3, 2, 1]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.reverse
}

pub fn is_empty_test() {
  assert list.is_empty([])

  assert !list.is_empty([1])
}

pub fn contains_test() {
  assert list.contains([0, 4, 5, 1], 1)

  assert !list.contains([0, 4, 5, 7], 1)

  assert !list.contains([], 1)

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.contains(1)
}

pub fn first_test() {
  assert list.first([0, 4, 5, 7]) == Ok(0)

  assert list.first([]) == Error(Nil)
}

pub fn rest_test() {
  assert list.rest([0, 4, 5, 7]) == Ok([4, 5, 7])

  assert list.rest([0]) == Ok([])

  assert list.rest([]) == Error(Nil)
}

pub fn group_test() {
  assert list.group([Ok(10), Error("Wrong"), Ok(200), Ok(1)], fn(i) {
      case i {
        Ok(_) -> "Successful"
        Error(_) -> "Failed"
      }
    })
    == dict.new()
    |> dict.insert("Failed", [Error("Wrong")])
    |> dict.insert("Successful", [Ok(1), Ok(200), Ok(10)])

  assert list.group([1, 2, 3, 4, 5], fn(i) { i - i / 3 * 3 })
    == dict.new()
    |> dict.insert(0, [3])
    |> dict.insert(1, [4, 1])
    |> dict.insert(2, [5, 2])
}

pub fn filter_test() {
  assert list.filter([], fn(_) { True }) == []

  assert list.filter([0, 4, 5, 7, 3], fn(_) { True }) == [0, 4, 5, 7, 3]

  assert list.filter([0, 4, 5, 7, 3], fn(x) { x > 4 }) == [5, 7]

  assert list.filter([0, 4, 5, 7, 3], fn(x) { x < 4 }) == [0, 3]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.filter(fn(x) { x == -1 })
}

pub fn filter_map_test() {
  assert list.filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) }) == [3, 5, 7, 2]

  assert list.filter_map([2, 4, 6, 1], Error) == []

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.filter_map(fn(x) { Ok(x + 1) })
}

pub fn map_test() {
  assert list.map([], fn(x) { x * 2 }) == []

  assert list.map([0, 4, 5, 7, 3], fn(x) { x * 2 }) == [0, 8, 10, 14, 6]

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.map(fn(x) { x })
}

pub fn map2_test() {
  assert list.map2([1, 2, 3], [], int.add) == []

  assert list.map2([], [1, 2, 3], int.add) == []

  assert list.map2([], [], int.add) == []

  assert list.map2([1, 2, 3], [4, 5], int.add) == [5, 7]

  assert list.map2([1, 2, 3], [4, 5, 6], int.add) == [5, 7, 9]

  assert list.map2([1, 2, 3], ["1", "2"], pair.new) == [#(1, "1"), #(2, "2")]

  // TCO test
  let list = list.repeat(0, recursion_test_cycles)
  list.map2(list, list, int.add)
}

pub fn map_fold_test() {
  assert list.map_fold([1, 2, 3, 4], from: 0, with: fn(acc, i) {
      #(acc + i, i * 2)
    })
    == #(10, [2, 4, 6, 8])

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

  assert list.try_map([5, 6, 5, 6], fun) == Ok([10, 12, 10, 12])

  assert list.try_map([4, 6, 5, 7, 3], fun) == Error(7)

  // TCO test
  list.repeat(6, recursion_test_cycles)
  |> list.try_map(fun)
}

pub fn drop_test() {
  assert list.drop([], 5) == []

  assert list.drop([1, 2, 3, 4, 5, 6, 7, 8], 5) == [6, 7, 8]

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.drop(recursion_test_cycles)
}

pub fn take_test() {
  assert list.take([], 5) == []

  assert list.take([1, 2, 3, 4, 5, 6, 7, 8], 5) == [1, 2, 3, 4, 5]

  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.take(recursion_test_cycles)
}

pub fn new_test() {
  assert list.new() == []
}

pub fn wrap_test() {
  assert list.wrap([]) == [[]]

  assert list.wrap([[]]) == [[[]]]

  assert list.wrap(Nil) == [Nil]

  assert list.wrap(1) == [1]

  assert list.wrap([1, 2]) == [[1, 2]]

  assert list.wrap([[1, 2, 3]]) == [[[1, 2, 3]]]
}

pub fn append_test() {
  assert list.append([1], [2, 3]) == [1, 2, 3]

  assert list.append([1, 2], []) == [1, 2]

  assert list.append([], [1, 2]) == [1, 2]

  assert list.append([1, 2], [3, 4]) == [1, 2, 3, 4]

  assert list.append([1, 2, 3], []) == [1, 2, 3]

  assert list.append([1, 2, 3], [4]) == [1, 2, 3, 4]

  assert list.append([1, 2, 3, 4], [5]) == [1, 2, 3, 4, 5]

  assert list.append([], []) == []

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.append([1])
}

pub fn flatten_test() {
  assert list.flatten([]) == []

  assert list.flatten([[]]) == []

  assert list.flatten([[], [], []]) == []

  assert list.flatten([[1, 2], [], [3, 4]]) == [1, 2, 3, 4]
}

pub fn flat_map_test() {
  assert list.flat_map([1, 10, 20], fn(x) { [x, x + 1] })
    == [1, 2, 10, 11, 20, 21]
}

pub fn fold_test() {
  assert list.fold([1, 2, 3], [], fn(acc, x) { [x, ..acc] }) == [3, 2, 1]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.fold([], fn(acc, x) { [x, ..acc] })
}

pub fn fold_right_test() {
  assert list.fold_right([1, 2, 3], from: [], with: fn(acc, x) { [x, ..acc] })
    == [1, 2, 3]
}

pub fn index_fold_test() {
  assert list.index_fold(["a", "b", "c"], [], fn(acc, i, ix) {
      [#(ix, i), ..acc]
    })
    == [#(2, "c"), #(1, "b"), #(0, "a")]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.index_fold([], fn(acc, i, ix) { [#(ix, i), ..acc] })
}

pub fn fold_until_test() {
  assert list.fold_until([1, 2, 3, 4], from: 0, with: fn(acc, n) {
      case n < 4 {
        True -> list.Continue(acc + n)
        False -> list.Stop(acc)
      }
    })
    == 6

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
  assert list.try_fold([1, 2, 3], 0, fn(acc, i) {
      case i < 4 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    })
    == Ok(6)

  assert list.try_fold([1, 2, 3], 0, fn(acc, i) {
      case i < 3 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    })
    == Error(Nil)

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

  assert list.find_map([1, 2, 3], with: f) == Ok(4)

  assert list.find_map([1, 3, 2], with: f) == Ok(4)

  assert list.find_map([1, 3], with: f) == Error(Nil)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.find_map(with: fn(x) {
    case x == recursion_test_cycles {
      True -> Ok(recursion_test_cycles)
      False -> Error(Nil)
    }
  })
}

pub fn find_test() {
  let is_two = fn(x) { x == 2 }

  assert list.find([1, 2, 3], one_that: is_two) == Ok(2)

  assert list.find([1, 3, 2], one_that: is_two) == Ok(2)

  assert list.find([1, 3], one_that: is_two) == Error(Nil)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.find(one_that: fn(x) { x == recursion_test_cycles })
}

pub fn all_test() {
  assert list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })

  assert !list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })

  assert list.all([], fn(_) { False })

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
  assert list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })

  assert !list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })

  assert !list.any([], fn(_) { False })

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
  assert list.zip([], [1, 2, 3]) == []

  assert list.zip([1, 2], []) == []

  assert list.zip([1, 2, 3], [4, 5, 6]) == [#(1, 4), #(2, 5), #(3, 6)]

  assert list.zip([5, 6], [1, 2, 3]) == [#(5, 1), #(6, 2)]

  assert list.zip([5, 6, 7], [1, 2]) == [#(5, 1), #(6, 2)]

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
}

pub fn strict_zip_test() {
  assert list.strict_zip([], [1, 2, 3]) == Error(Nil)

  assert list.strict_zip([1, 2], []) == Error(Nil)

  assert list.strict_zip([1, 2, 3], [4, 5, 6])
    == Ok([#(1, 4), #(2, 5), #(3, 6)])

  assert list.strict_zip([5, 6], [1, 2, 3]) == Error(Nil)

  assert list.strict_zip([5, 6, 7], [1, 2]) == Error(Nil)
}

pub fn unzip_test() {
  assert list.unzip([#(1, 2), #(3, 4)]) == #([1, 3], [2, 4])

  assert list.unzip([]) == #([], [])

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.unzip()
}

pub fn intersperse_test() {
  assert list.intersperse([1, 2, 3], 4) == [1, 4, 2, 4, 3]

  assert list.intersperse([], 2) == []

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.intersperse(0)
}

pub fn unique_test() {
  assert list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6]) == [1, 2, 3, 4, 5, 6]

  assert list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5]) == [7, 1, 45, 6, 2, 47, 5]

  assert list.unique([3, 4, 5]) == [3, 4, 5]

  assert list.unique([]) == []

  // TCO test
  list.range(0, recursion_test_cycles / 100)
  |> list.unique()
}

pub fn sort_test() {
  assert list.sort([], int.compare) == []

  assert list.sort([1], int.compare) == [1]

  assert list.sort([2, 1], int.compare) == [1, 2]

  assert list.sort([3, 1, 2], int.compare) == [1, 2, 3]

  assert list.sort([4, 3, 6, 5, 4], int.compare) == [3, 4, 4, 5, 6]

  assert list.sort([4, 3, 6, 5, 4, 1], int.compare) == [1, 3, 4, 4, 5, 6]

  assert list.sort([4.1, 3.1, 6.1, 5.1, 4.1], float.compare)
    == [3.1, 4.1, 4.1, 5.1, 6.1]

  assert list.sort([], int.compare) == []

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
  assert shuffled_cards
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
    == sorted_cards

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.reverse
  |> list.sort(int.compare)
}

pub fn index_map_test() {
  assert list.index_map([3, 4, 5], fn(x, i) { #(i, x) })
    == [#(0, 3), #(1, 4), #(2, 5)]

  let f = fn(x, i) { #(x, i) }
  assert list.index_map(["a", "b"], f) == [#("a", 0), #("b", 1)]

  let f = fn(x, i) { #(x, i) }
  assert list.index_map(["a", "b", "c"], f) == [#("a", 0), #("b", 1), #("c", 2)]
}

pub fn range_test() {
  assert list.range(0, 0) == [0]

  assert list.range(1, 1) == [1]

  assert list.range(-1, -1) == [-1]

  assert list.range(0, 1) == [0, 1]

  assert list.range(0, 5) == [0, 1, 2, 3, 4, 5]

  assert list.range(1, -5) == [1, 0, -1, -2, -3, -4, -5]

  // TCO test
  list.range(1, recursion_test_cycles)
}

pub fn repeat_test() {
  assert list.repeat(1, -10) == []

  assert list.repeat(1, 0) == []

  assert list.repeat(2, 3) == [2, 2, 2]

  assert list.repeat("x", 5) == ["x", "x", "x", "x", "x"]

  // TCO test
  list.repeat(0, recursion_test_cycles)
}

pub fn split_test() {
  assert list.split([], 0) == #([], [])

  assert list.split([0, 1, 2, 3, 4], 0) == #([], [0, 1, 2, 3, 4])

  assert list.split([0, 1, 2, 3, 4], -2) == #([], [0, 1, 2, 3, 4])

  assert list.split([0, 1, 2, 3, 4], 1) == #([0], [1, 2, 3, 4])

  assert list.split([0, 1, 2, 3, 4], 3) == #([0, 1, 2], [3, 4])

  assert list.split([0, 1, 2, 3, 4], 9) == #([0, 1, 2, 3, 4], [])

  // TCO test
  list.repeat(0, recursion_test_cycles + 10)
  |> list.split(recursion_test_cycles + 1)
}

pub fn split_while_test() {
  assert list.split_while([], fn(x) { x <= 5 }) == #([], [])

  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
    == #([1, 2, 3, 4, 5], [])

  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x == 2 })
    == #([], [1, 2, 3, 4, 5])

  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
    == #([1, 2, 3], [4, 5])

  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= -3 })
    == #([], [1, 2, 3, 4, 5])

  // TCO test
  list.repeat(0, recursion_test_cycles + 10)
  |> list.split_while(fn(x) { x <= recursion_test_cycles + 1 })
}

pub fn key_find_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  assert list.key_find(proplist, 0) == Ok("1")

  assert list.key_find(proplist, 1) == Ok("2")

  assert list.key_find(proplist, 2) == Error(Nil)
}

pub fn key_filter_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  assert list.key_filter(proplist, 0) == ["1", "3"]

  assert list.key_filter(proplist, 1) == ["2", "4"]

  assert list.key_filter(proplist, 2) == ["5"]

  assert list.key_filter(proplist, 3) == []
}

pub fn key_pop_test() {
  assert list.key_pop([#("a", 0), #("b", 1)], "a") == Ok(#(0, [#("b", 1)]))

  assert list.key_pop([#("a", 0), #("b", 1)], "b") == Ok(#(1, [#("a", 0)]))

  assert list.key_pop([#("a", 0), #("b", 1)], "c") == Error(Nil)
}

pub fn key_set_test() {
  assert list.key_set([#(5, 0), #(4, 1)], 4, 100) == [#(5, 0), #(4, 100)]

  assert list.key_set([#(5, 0), #(4, 1)], 1, 100)
    == [#(5, 0), #(4, 1), #(1, 100)]

  // TCO test
  let recursion_test_cycles_list = list.range(0, recursion_test_cycles)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.key_set(0, 0)
}

pub fn each_test() {
  assert list.each([1, 1, 1], fn(x) {
      let assert 1 = x
    })
    == Nil

  // TCO test
  list.each(list.repeat(1, recursion_test_cycles), fn(x) {
    let assert 1 = x
  })
}

pub fn try_each_test() {
  let assert Ok(Nil) =
    list.try_each(over: [1, 1, 1], with: fn(x) {
      assert x == 1
      Ok(Nil)
    })

  // `try_each` actually stops when `fun` returns error
  let assert Error(1) =
    list.try_each(over: [1, 2, 3], with: fn(x) {
      assert x == 1
      Error(x)
    })

  // TCO test
  let assert Ok(Nil) =
    list.repeat(1, recursion_test_cycles)
    |> list.try_each(with: fn(_) { Ok(Nil) })
}

pub fn partition_test() {
  assert list.partition([1, 2, 3, 4, 5, 6, 7], int.is_odd)
    == #([1, 3, 5, 7], [2, 4, 6])

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.partition(int.is_even)
}

pub fn permutations_test() {
  assert list.permutations([1, 2]) == [[1, 2], [2, 1]]

  assert list.permutations([1, 2, 3])
    == [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

  assert list.permutations([1, 2, 3, 4])
    == [
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
    ]

  assert list.permutations(["a", "b"]) == [["a", "b"], ["b", "a"]]

  assert list.permutations([1, 1]) == [[1, 1], [1, 1]]

  assert list.permutations([1, 1, 1])
    == [[1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1]]

  assert list.permutations([1, 2, 2])
    == [[1, 2, 2], [1, 2, 2], [2, 1, 2], [2, 2, 1], [2, 1, 2], [2, 2, 1]]

  assert list.permutations(["a", "a", "a", "a"])
    == [
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
    ]
}

pub fn window_test() {
  assert list.window([1, 2, 3], by: 2) == [[1, 2], [2, 3]]

  assert list.window([1, 2, 3], 3) == [[1, 2, 3]]

  assert list.window([1, 2, 3], 4) == []

  assert list.window([1, 2, 3, 4, 5], 3) == [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

  assert list.window([1, 2, 3], 0) == []

  assert list.window([1, 2, 3], -1) == []

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.window(2)
}

pub fn window_by_2_test() {
  assert list.window_by_2([1, 2, 3, 4]) == [#(1, 2), #(2, 3), #(3, 4)]

  assert list.window_by_2([1]) == []
}

pub fn drop_while_test() {
  assert list.drop_while([1, 2, 3, 4], fn(x) { x < 3 }) == [3, 4]

  // TCO test
  list.range(0, recursion_test_cycles + 10)
  |> list.drop_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn take_while_test() {
  assert list.take_while([1, 2, 3, 2, 4], fn(x) { x < 3 }) == [1, 2]

  // TCO test
  list.range(0, recursion_test_cycles + 10)
  |> list.take_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn chunk_test() {
  assert list.chunk([1, 2, 3], by: fn(n) { n % 2 }) == [[1], [2], [3]]

  assert list.chunk([1, 2, 2, 3, 4, 4, 6, 7, 7], by: fn(n) { n % 2 })
    == [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.chunk(by: fn(n) { n % 2 })
}

pub fn sized_chunk_test() {
  assert list.sized_chunk([1, 2, 3, 4, 5, 6], into: 2)
    == [[1, 2], [3, 4], [5, 6]]

  assert list.sized_chunk([1, 2, 3, 4, 5, 6, 7, 8], into: 3)
    == [[1, 2, 3], [4, 5, 6], [7, 8]]

  // TCO test
  list.range(0, recursion_test_cycles * 3)
  |> list.sized_chunk(into: 3)
}

pub fn reduce_test() {
  assert list.reduce([], with: fn(x, y) { x + y }) == Error(Nil)

  assert list.reduce([1, 2, 3, 4, 5], with: fn(x, y) { x + y }) == Ok(15)
}

pub fn scan_test() {
  assert list.scan([], from: 0, with: fn(acc, i) { i + acc }) == []

  assert list.scan([1, 2, 3, 4], from: 0, with: fn(acc, i) { 2 * i + acc })
    == [2, 6, 12, 20]

  assert list.scan([1, 2, 3, 4], from: [], with: fn(acc, i) {
      case int.is_even(i) {
        True -> ["Even", ..acc]
        False -> ["Odd", ..acc]
      }
    })
    == [
      ["Odd"],
      ["Even", "Odd"],
      ["Odd", "Even", "Odd"],
      ["Even", "Odd", "Even", "Odd"],
    ]

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.scan(from: 0, with: fn(acc, i) { i + acc })
}

pub fn last_test() {
  assert list.last([]) == Error(Nil)

  assert list.last([1, 2, 3, 4, 5]) == Ok(5)
}

pub fn combinations_test() {
  assert list.combinations([1, 2, 3], by: 0) == [[]]

  assert list.combinations([1, 2, 3], by: 1) == [[1], [2], [3]]

  assert list.combinations([1, 2, 3], by: 2) == [[1, 2], [1, 3], [2, 3]]

  assert list.combinations([1, 2, 3], by: 3) == [[1, 2, 3]]

  assert list.combinations([1, 2, 3], by: 4) == []

  assert list.combinations([1, 2, 3, 4], 3)
    == [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
}

pub fn combination_pairs_test() {
  assert list.combination_pairs([1]) == []

  assert list.combination_pairs([1, 2]) == [#(1, 2)]

  assert list.combination_pairs([1, 2, 3]) == [#(1, 2), #(1, 3), #(2, 3)]

  assert list.combination_pairs([1, 2, 3, 4])
    == [#(1, 2), #(1, 3), #(1, 4), #(2, 3), #(2, 4), #(3, 4)]

  // TCO test
  list.range(0, 200)
  |> list.combination_pairs()
}

pub fn interleave_test() {
  assert list.interleave([[1, 2], [101, 102]]) == [1, 101, 2, 102]

  assert list.interleave([[1, 2], [101, 102], [201, 202]])
    == [1, 101, 201, 2, 102, 202]

  // Left over elements are added at the end
  assert list.interleave([[1, 2, 3], [101, 102]]) == [1, 101, 2, 102, 3]

  assert list.interleave([[1, 2], [101, 102, 103]]) == [1, 101, 2, 102, 103]
}

pub fn transpose_test() {
  assert list.transpose([[1, 2, 3], [101, 102, 103]])
    == [[1, 101], [2, 102], [3, 103]]

  assert list.transpose([[1, 2, 3], [101, 102, 103], [201, 202, 203]])
    == [[1, 101, 201], [2, 102, 202], [3, 103, 203]]

  // Left over elements are still returned
  assert list.transpose([[1, 2], [101, 102, 103]])
    == [[1, 101], [2, 102], [103]]

  assert list.transpose([[1, 2, 3], [101, 102], [201, 202, 203]])
    == [[1, 101, 201], [2, 102, 202], [3, 203]]
}

pub fn shuffle_test() {
  assert list.shuffle([]) == []

  assert list.shuffle([1, 1]) == [1, 1]

  assert list.shuffle([1, 1, 1]) == [1, 1, 1]

  assert list.range(1, 100)
    |> list.shuffle
    |> list.sort(int.compare)
    == list.range(1, 100)

  // TCO test
  list.range(0, recursion_test_cycles)
  |> list.shuffle()
}

pub fn max_test() {
  assert list.max([], int.compare) == Error(Nil)

  assert list.max([1, 3, 2], int.compare) == Ok(3)

  assert list.max([-1.0, 1.2, 1.104], float.compare) == Ok(1.2)

  assert list.max(["a", "c", "b"], string.compare) == Ok("c")

  assert list.max([1, 1], int.compare) == Ok(1)
}

pub fn min_test() {
  assert list.min([], int.compare) == Error(Nil)

  assert list.min([1, 3, 2], int.compare) == Ok(1)

  assert list.min([-1.0, 1.2, 1.104], float.compare) == Ok(-1.0)

  assert list.min(["a", "c", "b"], string.compare) == Ok("a")

  assert list.min([1, 1], int.compare) == Ok(1)
}

pub fn sample_test() {
  assert list.sample([], 3) == []
  assert list.sample([1, 2, 3], 0) == []
  assert list.sample([1, 2, 3], -1) == []
  assert list.sort(list.sample([1, 2], 5), int.compare) == [1, 2]
  assert list.sample([1], 1) == [1]

  assert 10
    == list.range(1, 100)
    |> list.sample(10)
    |> list.unique
    |> list.length

  assert [1, 1, 1, 1, 1]
    |> list.sample(3)
    |> list.all(fn(x) { x == 1 })

  // Some tests on a bigger sample.
  let sample =
    list.range(1, 1000)
    |> list.sample(100)

  assert sample
    |> list.sort(int.compare)
    |> list.all(fn(x) { x >= 1 && x <= 1000 })

  assert 100 == list.length(list.unique(sample))

  let assert Ok(min) = list.reduce(sample, int.min)
  let assert Ok(max) = list.reduce(sample, int.max)
  assert min >= 1
  assert max <= 1000
}

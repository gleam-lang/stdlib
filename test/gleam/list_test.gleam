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

pub fn length_empty_test() {
  let zero = 0
  assert list.length([]) == zero
}

pub fn length_one_test() {
  assert list.length([1]) == 1
}

pub fn length_two_test() {
  assert list.length([1, 1]) == 2
}

pub fn length_three_test() {
  assert list.length([1, 1, 1]) == 3
}

pub fn length_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.length()
}

pub fn count_empty_test() {
  assert list.count([], int.is_odd) == 0
}

pub fn count_no_matches_test() {
  assert list.count([2, 4, 6], int.is_odd) == 0
}

pub fn count_some_matches_test() {
  assert list.count([1, 2, 3, 4, 5], int.is_odd) == 3
}

pub fn count_strings_test() {
  assert list.count(["a", "list", "with", "some", "string", "values"], fn(a) {
      string.length(a) > 4
    })
    == 2
}

pub fn reverse_empty_test() {
  assert list.reverse([]) == []
}

pub fn reverse_one_test() {
  assert list.reverse([1]) == [1]
}

pub fn reverse_two_test() {
  assert list.reverse([1, 2]) == [2, 1]
}

pub fn reverse_five_test() {
  assert list.reverse([1, 2, 3, 4, 5]) == [5, 4, 3, 2, 1]
}

pub fn reverse_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.reverse
}

pub fn is_empty_empty_test() {
  assert list.is_empty([])
}

pub fn is_empty_non_empty_test() {
  assert !list.is_empty([1])
}

pub fn contains_present_test() {
  assert list.contains([0, 4, 5, 1], 1)
}

pub fn contains_absent_test() {
  assert !list.contains([0, 4, 5, 7], 1)
}

pub fn contains_empty_test() {
  assert !list.contains([], 1)
}

pub fn contains_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.contains(1)
}

pub fn first_ok_test() {
  assert list.first([0, 4, 5, 7]) == Ok(0)
}

pub fn first_error_test() {
  assert list.first([]) == Error(Nil)
}

pub fn rest_ok_test() {
  assert list.rest([0, 4, 5, 7]) == Ok([4, 5, 7])
}

pub fn rest_single_test() {
  assert list.rest([0]) == Ok([])
}

pub fn rest_error_test() {
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
}

pub fn group_modulo_test() {
  assert list.group([1, 2, 3, 4, 5], fn(i) { i - i / 3 * 3 })
    == dict.new()
    |> dict.insert(0, [3])
    |> dict.insert(1, [4, 1])
    |> dict.insert(2, [5, 2])
}

pub fn filter_empty_test() {
  assert list.filter([], fn(_) { True }) == []
}

pub fn filter_all_true_test() {
  assert list.filter([0, 4, 5, 7, 3], fn(_) { True }) == [0, 4, 5, 7, 3]
}

pub fn filter_greater_than_test() {
  assert list.filter([0, 4, 5, 7, 3], fn(x) { x > 4 }) == [5, 7]
}

pub fn filter_less_than_test() {
  assert list.filter([0, 4, 5, 7, 3], fn(x) { x < 4 }) == [0, 3]
}

pub fn filter_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.filter(fn(x) { x == -1 })
}

pub fn filter_map_ok_test() {
  assert list.filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) }) == [3, 5, 7, 2]
}

pub fn filter_map_error_test() {
  assert list.filter_map([2, 4, 6, 1], Error) == []
}

pub fn filter_map_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.filter_map(fn(x) { Ok(x + 1) })
}

pub fn map_empty_test() {
  assert list.map([], fn(x) { x * 2 }) == []
}

pub fn map_double_test() {
  assert list.map([0, 4, 5, 7, 3], fn(x) { x * 2 }) == [0, 8, 10, 14, 6]
}

pub fn map_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.map(fn(x) { x })
}

pub fn map2_empty_second_test() {
  assert list.map2([1, 2, 3], [], int.add) == []
}

pub fn map2_empty_first_test() {
  assert list.map2([], [1, 2, 3], int.add) == []
}

pub fn map2_both_empty_test() {
  assert list.map2([], [], int.add) == []
}

pub fn map2_shorter_second_test() {
  assert list.map2([1, 2, 3], [4, 5], int.add) == [5, 7]
}

pub fn map2_equal_length_test() {
  assert list.map2([1, 2, 3], [4, 5, 6], int.add) == [5, 7, 9]
}

pub fn map2_pair_test() {
  assert list.map2([1, 2, 3], ["1", "2"], pair.new) == [#(1, "1"), #(2, "2")]
}

pub fn map2_tco_test() {
  // TCO test
  let list = list.repeat(0, recursion_test_cycles)
  list.map2(list, list, int.add)
}

pub fn map_fold_test() {
  assert list.map_fold([1, 2, 3, 4], from: 0, with: fn(acc, i) {
      #(acc + i, i * 2)
    })
    == #(10, [2, 4, 6, 8])
}

pub fn map_fold_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.map_fold(from: 0, with: fn(acc, i) { #(acc + i, i + 1) })
}

pub fn try_map_ok_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  assert list.try_map([5, 6, 5, 6], fun) == Ok([10, 12, 10, 12])
}

pub fn try_map_error_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  assert list.try_map([4, 6, 5, 7, 3], fun) == Error(7)
}

pub fn try_map_tco_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  // TCO test
  list.repeat(6, recursion_test_cycles)
  |> list.try_map(fun)
}

pub fn drop_empty_test() {
  assert list.drop([], 5) == []
}

pub fn drop_five_test() {
  assert list.drop([1, 2, 3, 4, 5, 6, 7, 8], 5) == [6, 7, 8]
}

pub fn drop_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.drop(recursion_test_cycles)
}

pub fn take_empty_test() {
  assert list.take([], 5) == []
}

pub fn take_five_test() {
  assert list.take([1, 2, 3, 4, 5, 6, 7, 8], 5) == [1, 2, 3, 4, 5]
}

pub fn take_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.take(recursion_test_cycles)
}

pub fn new_test() {
  assert list.new() == []
}

pub fn wrap_empty_test() {
  assert list.wrap([]) == [[]]
}

pub fn wrap_nested_empty_test() {
  assert list.wrap([[]]) == [[[]]]
}

pub fn wrap_nil_test() {
  assert list.wrap(Nil) == [Nil]
}

pub fn wrap_int_test() {
  assert list.wrap(1) == [1]
}

pub fn wrap_list_test() {
  assert list.wrap([1, 2]) == [[1, 2]]
}

pub fn wrap_nested_list_test() {
  assert list.wrap([[1, 2, 3]]) == [[[1, 2, 3]]]
}

pub fn append_basic_test() {
  assert list.append([1], [2, 3]) == [1, 2, 3]
}

pub fn append_empty_second_test() {
  assert list.append([1, 2], []) == [1, 2]
}

pub fn append_empty_first_test() {
  assert list.append([], [1, 2]) == [1, 2]
}

pub fn append_equal_length_test() {
  assert list.append([1, 2], [3, 4]) == [1, 2, 3, 4]
}

pub fn append_longer_first_test() {
  assert list.append([1, 2, 3], []) == [1, 2, 3]
}

pub fn append_single_second_test() {
  assert list.append([1, 2, 3], [4]) == [1, 2, 3, 4]
}

pub fn append_four_one_test() {
  assert list.append([1, 2, 3, 4], [5]) == [1, 2, 3, 4, 5]
}

pub fn append_both_empty_test() {
  assert list.append([], []) == []
}

pub fn append_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.append([1])
}

pub fn flatten_empty_test() {
  assert list.flatten([]) == []
}

pub fn flatten_single_empty_test() {
  assert list.flatten([[]]) == []
}

pub fn flatten_multiple_empty_test() {
  assert list.flatten([[], [], []]) == []
}

pub fn flatten_mixed_test() {
  assert list.flatten([[1, 2], [], [3, 4]]) == [1, 2, 3, 4]
}

pub fn flat_map_test() {
  assert list.flat_map([1, 10, 20], fn(x) { [x, x + 1] })
    == [1, 2, 10, 11, 20, 21]
}

pub fn fold_test() {
  assert list.fold([1, 2, 3], [], fn(acc, x) { [x, ..acc] }) == [3, 2, 1]
}

pub fn fold_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
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
}

pub fn index_fold_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
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
}

pub fn fold_until_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.fold_until(from: 0, with: fn(acc, n) {
    case n < recursion_test_cycles {
      True -> list.Continue(acc + n)
      False -> list.Stop(acc)
    }
  })
}

pub fn try_fold_ok_test() {
  assert list.try_fold([1, 2, 3], 0, fn(acc, i) {
      case i < 4 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    })
    == Ok(6)
}

pub fn try_fold_error_test() {
  assert list.try_fold([1, 2, 3], 0, fn(acc, i) {
      case i < 3 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    })
    == Error(Nil)
}

pub fn try_fold_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.try_fold(0, fn(acc, i) {
    case i < recursion_test_cycles {
      True -> Ok(acc + i)
      False -> Error(Nil)
    }
  })
}

pub fn find_map_found_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  assert list.find_map([1, 2, 3], with: f) == Ok(4)
}

pub fn find_map_found_different_order_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  assert list.find_map([1, 3, 2], with: f) == Ok(4)
}

pub fn find_map_not_found_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  assert list.find_map([1, 3], with: f) == Error(Nil)
}

pub fn find_map_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.find_map(with: fn(x) {
    case x == recursion_test_cycles {
      True -> Ok(recursion_test_cycles)
      False -> Error(Nil)
    }
  })
}

pub fn find_found_test() {
  let is_two = fn(x) { x == 2 }

  assert list.find([1, 2, 3], one_that: is_two) == Ok(2)
}

pub fn find_found_different_order_test() {
  let is_two = fn(x) { x == 2 }

  assert list.find([1, 3, 2], one_that: is_two) == Ok(2)
}

pub fn find_not_found_test() {
  let is_two = fn(x) { x == 2 }

  assert list.find([1, 3], one_that: is_two) == Error(Nil)
}

pub fn find_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.find(one_that: fn(x) { x == recursion_test_cycles })
}

pub fn all_true_test() {
  assert list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })
}

pub fn all_false_test() {
  assert !list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })
}

pub fn all_empty_test() {
  assert list.all([], fn(_) { False })
}

pub fn all_short_circuit_test() {
  [1, 2, 3]
  |> list.all(fn(x) {
    case x {
      1 -> True
      2 -> False
      // Crash if no short-circuit
      _ -> panic
    }
  })
}

pub fn all_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
  |> list.all(fn(x) {
    case x {
      0 -> True
      _ -> False
    }
  })
}

pub fn any_true_test() {
  assert list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })
}

pub fn any_false_test() {
  assert !list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })
}

pub fn any_empty_test() {
  assert !list.any([], fn(_) { False })
}

pub fn any_short_circuit_test() {
  [1, 2, 3]
  |> list.any(fn(x) {
    case x {
      1 -> False
      2 -> True
      // Crash if no short-circuit
      _ -> panic
    }
  })
}

pub fn any_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.any(fn(x) { x == recursion_test_cycles })
}

pub fn zip_empty_second_test() {
  assert list.zip([], [1, 2, 3]) == []
}

pub fn zip_empty_first_test() {
  assert list.zip([1, 2], []) == []
}

pub fn zip_equal_length_test() {
  assert list.zip([1, 2, 3], [4, 5, 6]) == [#(1, 4), #(2, 5), #(3, 6)]
}

pub fn zip_shorter_second_test() {
  assert list.zip([5, 6], [1, 2, 3]) == [#(5, 1), #(6, 2)]
}

pub fn zip_shorter_first_test() {
  assert list.zip([5, 6, 7], [1, 2]) == [#(5, 1), #(6, 2)]
}

pub fn zip_tco_test() {
  // TCO test
  let recursion_test_cycles_list =
    int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
}

pub fn strict_zip_empty_second_error_test() {
  assert list.strict_zip([], [1, 2, 3]) == Error(Nil)
}

pub fn strict_zip_empty_first_error_test() {
  assert list.strict_zip([1, 2], []) == Error(Nil)
}

pub fn strict_zip_equal_length_ok_test() {
  assert list.strict_zip([1, 2, 3], [4, 5, 6])
    == Ok([#(1, 4), #(2, 5), #(3, 6)])
}

pub fn strict_zip_shorter_second_error_test() {
  assert list.strict_zip([5, 6], [1, 2, 3]) == Error(Nil)
}

pub fn strict_zip_shorter_first_error_test() {
  assert list.strict_zip([5, 6, 7], [1, 2]) == Error(Nil)
}

pub fn unzip_test() {
  assert list.unzip([#(1, 2), #(3, 4)]) == #([1, 3], [2, 4])
}

pub fn unzip_empty_test() {
  assert list.unzip([]) == #([], [])
}

pub fn unzip_tco_test() {
  // TCO test
  let recursion_test_cycles_list =
    int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.unzip()
}

pub fn intersperse_test() {
  assert list.intersperse([1, 2, 3], 4) == [1, 4, 2, 4, 3]
}

pub fn intersperse_empty_test() {
  assert list.intersperse([], 2) == []
}

pub fn intersperse_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.intersperse(0)
}

pub fn unique_duplicates_test() {
  assert list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6]) == [1, 2, 3, 4, 5, 6]
}

pub fn unique_scattered_test() {
  assert list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5]) == [7, 1, 45, 6, 2, 47, 5]
}

pub fn unique_no_duplicates_test() {
  assert list.unique([3, 4, 5]) == [3, 4, 5]
}

pub fn unique_empty_test() {
  assert list.unique([]) == []
}

pub fn unique_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles / 100,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.unique()
}

pub fn sort_empty_test() {
  assert list.sort([], int.compare) == []
}

pub fn sort_single_test() {
  assert list.sort([1], int.compare) == [1]
}

pub fn sort_two_test() {
  assert list.sort([2, 1], int.compare) == [1, 2]
}

pub fn sort_three_test() {
  assert list.sort([3, 1, 2], int.compare) == [1, 2, 3]
}

pub fn sort_five_test() {
  assert list.sort([4, 3, 6, 5, 4], int.compare) == [3, 4, 4, 5, 6]
}

pub fn sort_six_test() {
  assert list.sort([4, 3, 6, 5, 4, 1], int.compare) == [1, 3, 4, 4, 5, 6]
}

pub fn sort_float_test() {
  assert list.sort([4.1, 3.1, 6.1, 5.1, 4.1], float.compare)
    == [3.1, 4.1, 4.1, 5.1, 6.1]
}

pub fn sort_empty_2_test() {
  assert list.sort([], int.compare) == []
}

pub fn sort_stability_test() {
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
}

pub fn sort_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.reverse
  |> list.sort(int.compare)
}

pub fn index_map_test() {
  assert list.index_map([3, 4, 5], fn(x, i) { #(i, x) })
    == [#(0, 3), #(1, 4), #(2, 5)]
}

pub fn index_map_strings_test() {
  let f = fn(x, i) { #(x, i) }
  assert list.index_map(["a", "b"], f) == [#("a", 0), #("b", 1)]
}

pub fn index_map_three_strings_test() {
  let f = fn(x, i) { #(x, i) }
  assert list.index_map(["a", "b", "c"], f) == [#("a", 0), #("b", 1), #("c", 2)]
}

pub fn repeat_negative_test() {
  assert list.repeat(1, -10) == []
}

pub fn repeat_zero_test() {
  assert list.repeat(1, 0) == []
}

pub fn repeat_three_test() {
  assert list.repeat(2, 3) == [2, 2, 2]
}

pub fn repeat_five_test() {
  assert list.repeat("x", 5) == ["x", "x", "x", "x", "x"]
}

pub fn repeat_tco_test() {
  // TCO test
  list.repeat(0, recursion_test_cycles)
}

pub fn split_zero_test() {
  assert list.split([], 0) == #([], [])
}

pub fn split_at_zero_test() {
  assert list.split([0, 1, 2, 3, 4], 0) == #([], [0, 1, 2, 3, 4])
}

pub fn split_negative_test() {
  assert list.split([0, 1, 2, 3, 4], -2) == #([], [0, 1, 2, 3, 4])
}

pub fn split_at_one_test() {
  assert list.split([0, 1, 2, 3, 4], 1) == #([0], [1, 2, 3, 4])
}

pub fn split_at_three_test() {
  assert list.split([0, 1, 2, 3, 4], 3) == #([0, 1, 2], [3, 4])
}

pub fn split_beyond_length_test() {
  assert list.split([0, 1, 2, 3, 4], 9) == #([0, 1, 2, 3, 4], [])
}

pub fn split_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles + 10,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.split(recursion_test_cycles + 1)
}

pub fn split_while_empty_test() {
  assert list.split_while([], fn(x) { x <= 5 }) == #([], [])
}

pub fn split_while_all_match_test() {
  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
    == #([1, 2, 3, 4, 5], [])
}

pub fn split_while_none_match_test() {
  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x == 2 })
    == #([], [1, 2, 3, 4, 5])
}

pub fn split_while_partial_match_test() {
  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
    == #([1, 2, 3], [4, 5])
}

pub fn split_while_negative_condition_test() {
  assert list.split_while([1, 2, 3, 4, 5], fn(x) { x <= -3 })
    == #([], [1, 2, 3, 4, 5])
}

pub fn split_while_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles + 10,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.split_while(fn(x) { x <= recursion_test_cycles + 1 })
}

pub fn key_find_first_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  assert list.key_find(proplist, 0) == Ok("1")
}

pub fn key_find_second_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  assert list.key_find(proplist, 1) == Ok("2")
}

pub fn key_find_not_found_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  assert list.key_find(proplist, 2) == Error(Nil)
}

pub fn key_filter_zero_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  assert list.key_filter(proplist, 0) == ["1", "3"]
}

pub fn key_filter_one_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  assert list.key_filter(proplist, 1) == ["2", "4"]
}

pub fn key_filter_two_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  assert list.key_filter(proplist, 2) == ["5"]
}

pub fn key_filter_not_found_test() {
  let proplist = [#(0, "1"), #(1, "2"), #(0, "3"), #(1, "4"), #(2, "5")]

  assert list.key_filter(proplist, 3) == []
}

pub fn key_pop_a_test() {
  assert list.key_pop([#("a", 0), #("b", 1)], "a") == Ok(#(0, [#("b", 1)]))
}

pub fn key_pop_b_test() {
  assert list.key_pop([#("a", 0), #("b", 1)], "b") == Ok(#(1, [#("a", 0)]))
}

pub fn key_pop_not_found_test() {
  assert list.key_pop([#("a", 0), #("b", 1)], "c") == Error(Nil)
}

pub fn key_set_existing_test() {
  assert list.key_set([#(5, 0), #(4, 1)], 4, 100) == [#(5, 0), #(4, 100)]
}

pub fn key_set_new_test() {
  assert list.key_set([#(5, 0), #(4, 1)], 1, 100)
    == [#(5, 0), #(4, 1), #(1, 100)]
}

pub fn key_set_tco_test() {
  // TCO test
  let recursion_test_cycles_list =
    int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  list.zip(recursion_test_cycles_list, recursion_test_cycles_list)
  |> list.key_set(0, 0)
}

pub fn each_test() {
  assert list.each([1, 1, 1], fn(x) {
      let assert 1 = x
    })
    == Nil
}

pub fn each_tco_test() {
  // TCO test
  list.each(list.repeat(1, recursion_test_cycles), fn(x) {
    let assert 1 = x
  })
}

pub fn try_each_ok_test() {
  let assert Ok(Nil) =
    list.try_each(over: [1, 1, 1], with: fn(x) {
      assert x == 1
      Ok(Nil)
    })
}

pub fn try_each_stops_on_error_test() {
  // `try_each` actually stops when `fun` returns error
  let assert Error(1) =
    list.try_each(over: [1, 2, 3], with: fn(x) {
      assert x == 1
      Error(x)
    })
}

pub fn try_each_tco_test() {
  // TCO test
  let assert Ok(Nil) =
    list.repeat(1, recursion_test_cycles)
    |> list.try_each(with: fn(_) { Ok(Nil) })
}

pub fn partition_test() {
  assert list.partition([1, 2, 3, 4, 5, 6, 7], int.is_odd)
    == #([1, 3, 5, 7], [2, 4, 6])
}

pub fn partition_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.partition(int.is_even)
}

pub fn permutations_two_test() {
  assert list.permutations([1, 2]) == [[1, 2], [2, 1]]
}

pub fn permutations_three_test() {
  assert list.permutations([1, 2, 3])
    == [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
}

pub fn permutations_four_test() {
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
}

pub fn permutations_strings_test() {
  assert list.permutations(["a", "b"]) == [["a", "b"], ["b", "a"]]
}

pub fn permutations_duplicates_two_test() {
  assert list.permutations([1, 1]) == [[1, 1], [1, 1]]
}

pub fn permutations_duplicates_three_test() {
  assert list.permutations([1, 1, 1])
    == [[1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1]]
}

pub fn permutations_partial_duplicates_test() {
  assert list.permutations([1, 2, 2])
    == [[1, 2, 2], [1, 2, 2], [2, 1, 2], [2, 2, 1], [2, 1, 2], [2, 2, 1]]
}

pub fn permutations_all_same_four_test() {
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

pub fn window_two_test() {
  assert list.window([1, 2, 3], by: 2) == [[1, 2], [2, 3]]
}

pub fn window_three_test() {
  assert list.window([1, 2, 3], 3) == [[1, 2, 3]]
}

pub fn window_four_test() {
  assert list.window([1, 2, 3], 4) == []
}

pub fn window_five_elements_test() {
  assert list.window([1, 2, 3, 4, 5], 3) == [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
}

pub fn window_zero_test() {
  assert list.window([1, 2, 3], 0) == []
}

pub fn window_negative_test() {
  assert list.window([1, 2, 3], -1) == []
}

pub fn window_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.window(2)
}

pub fn window_by_2_test() {
  assert list.window_by_2([1, 2, 3, 4]) == [#(1, 2), #(2, 3), #(3, 4)]
}

pub fn window_by_2_single_test() {
  assert list.window_by_2([1]) == []
}

pub fn drop_while_test() {
  assert list.drop_while([1, 2, 3, 4], fn(x) { x < 3 }) == [3, 4]
}

pub fn drop_while_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles + 10,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.drop_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn take_while_test() {
  assert list.take_while([1, 2, 3, 2, 4], fn(x) { x < 3 }) == [1, 2]
}

pub fn take_while_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles + 10,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.take_while(fn(x) { x < recursion_test_cycles + 1 })
}

pub fn chunk_test() {
  assert list.chunk([1, 2, 3], by: fn(n) { n % 2 }) == [[1], [2], [3]]
}

pub fn chunk_adjacent_test() {
  assert list.chunk([1, 2, 2, 3, 4, 4, 6, 7, 7], by: fn(n) { n % 2 })
    == [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
}

pub fn chunk_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.chunk(by: fn(n) { n % 2 })
}

pub fn sized_chunk_test() {
  assert list.sized_chunk([1, 2, 3, 4, 5, 6], into: 2)
    == [[1, 2], [3, 4], [5, 6]]
}

pub fn sized_chunk_remainder_test() {
  assert list.sized_chunk([1, 2, 3, 4, 5, 6, 7, 8], into: 3)
    == [[1, 2, 3], [4, 5, 6], [7, 8]]
}

pub fn sized_chunk_tco_test() {
  // TCO test
  int.range(
    from: recursion_test_cycles * 3,
    to: -1,
    with: [],
    run: list.prepend,
  )
  |> list.sized_chunk(into: 3)
}

pub fn reduce_empty_test() {
  assert list.reduce([], with: fn(x, y) { x + y }) == Error(Nil)
}

pub fn reduce_sum_test() {
  assert list.reduce([1, 2, 3, 4, 5], with: fn(x, y) { x + y }) == Ok(15)
}

pub fn scan_empty_test() {
  assert list.scan([], from: 0, with: fn(acc, i) { i + acc }) == []
}

pub fn scan_basic_test() {
  assert list.scan([1, 2, 3, 4], from: 0, with: fn(acc, i) { 2 * i + acc })
    == [2, 6, 12, 20]
}

pub fn scan_list_test() {
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
}

pub fn scan_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.scan(from: 0, with: fn(acc, i) { i + acc })
}

pub fn last_empty_test() {
  assert list.last([]) == Error(Nil)
}

pub fn last_ok_test() {
  assert list.last([1, 2, 3, 4, 5]) == Ok(5)
}

pub fn combinations_zero_test() {
  assert list.combinations([1, 2, 3], by: 0) == [[]]
}

pub fn combinations_one_test() {
  assert list.combinations([1, 2, 3], by: 1) == [[1], [2], [3]]
}

pub fn combinations_two_test() {
  assert list.combinations([1, 2, 3], by: 2) == [[1, 2], [1, 3], [2, 3]]
}

pub fn combinations_three_test() {
  assert list.combinations([1, 2, 3], by: 3) == [[1, 2, 3]]
}

pub fn combinations_four_test() {
  assert list.combinations([1, 2, 3], by: 4) == []
}

pub fn combinations_four_elements_test() {
  assert list.combinations([1, 2, 3, 4], 3)
    == [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
}

pub fn combination_pairs_single_test() {
  assert list.combination_pairs([1]) == []
}

pub fn combination_pairs_two_test() {
  assert list.combination_pairs([1, 2]) == [#(1, 2)]
}

pub fn combination_pairs_three_test() {
  assert list.combination_pairs([1, 2, 3]) == [#(1, 2), #(1, 3), #(2, 3)]
}

pub fn combination_pairs_four_test() {
  assert list.combination_pairs([1, 2, 3, 4])
    == [#(1, 2), #(1, 3), #(1, 4), #(2, 3), #(2, 4), #(3, 4)]
}

pub fn combination_pairs_tco_test() {
  // TCO test
  int.range(from: 200, to: -1, with: [], run: list.prepend)
  |> list.combination_pairs()
}

pub fn interleave_two_test() {
  assert list.interleave([[1, 2], [101, 102]]) == [1, 101, 2, 102]
}

pub fn interleave_three_test() {
  assert list.interleave([[1, 2], [101, 102], [201, 202]])
    == [1, 101, 201, 2, 102, 202]
}

pub fn interleave_leftover_first_test() {
  // Left over elements are added at the end
  assert list.interleave([[1, 2, 3], [101, 102]]) == [1, 101, 2, 102, 3]
}

pub fn interleave_leftover_second_test() {
  assert list.interleave([[1, 2], [101, 102, 103]]) == [1, 101, 2, 102, 103]
}

pub fn transpose_two_test() {
  assert list.transpose([[1, 2, 3], [101, 102, 103]])
    == [[1, 101], [2, 102], [3, 103]]
}

pub fn transpose_three_test() {
  assert list.transpose([[1, 2, 3], [101, 102, 103], [201, 202, 203]])
    == [[1, 101, 201], [2, 102, 202], [3, 103, 203]]
}

pub fn transpose_leftover_second_test() {
  // Left over elements are still returned
  assert list.transpose([[1, 2], [101, 102, 103]])
    == [[1, 101], [2, 102], [103]]
}

pub fn transpose_leftover_mixed_test() {
  assert list.transpose([[1, 2, 3], [101, 102], [201, 202, 203]])
    == [[1, 101, 201], [2, 102, 202], [3, 203]]
}

pub fn shuffle_empty_test() {
  assert list.shuffle([]) == []
}

pub fn shuffle_two_same_test() {
  assert list.shuffle([1, 1]) == [1, 1]
}

pub fn shuffle_three_same_test() {
  assert list.shuffle([1, 1, 1]) == [1, 1, 1]
}

pub fn shuffle_preserves_elements_test() {
  let one_to_hundred = int.range(from: 100, to: 0, with: [], run: list.prepend)
  assert one_to_hundred
    |> list.shuffle
    |> list.sort(int.compare)
    == one_to_hundred
}

pub fn shuffle_tco_test() {
  // TCO test
  int.range(from: recursion_test_cycles, to: -1, with: [], run: list.prepend)
  |> list.shuffle()
}

pub fn max_empty_test() {
  assert list.max([], int.compare) == Error(Nil)
}

pub fn max_int_test() {
  assert list.max([1, 3, 2], int.compare) == Ok(3)
}

pub fn max_float_test() {
  assert list.max([-1.0, 1.2, 1.104], float.compare) == Ok(1.2)
}

pub fn max_string_test() {
  assert list.max(["a", "c", "b"], string.compare) == Ok("c")
}

pub fn sample_empty_test() {
  assert list.sample([], 3) == []
}

pub fn sample_zero_test() {
  assert list.sample([1, 2, 3], 0) == []
}

pub fn sample_negative_test() {
  assert list.sample([1, 2, 3], -1) == []
}

pub fn sample_larger_than_list_test() {
  assert list.sort(list.sample([1, 2], 5), int.compare) == [1, 2]
}

pub fn sample_single_test() {
  assert list.sample([1], 1) == [1]
}

pub fn sample_uniqueness_test() {
  assert 10
    == int.range(from: 100, to: 0, with: [], run: list.prepend)
    |> list.sample(10)
    |> list.unique
    |> list.length
}

pub fn sample_same_value_test() {
  assert [1, 1, 1, 1, 1]
    |> list.sample(3)
    |> list.all(fn(x) { x == 1 })
}

pub fn sample_range_test() {
  // Some tests on a bigger sample.
  let sample =
    int.range(from: 1000, to: 0, with: [], run: list.prepend)
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

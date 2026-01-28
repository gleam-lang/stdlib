import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

pub fn from_list_size_test() {
  assert [#(4, 0), #(1, 0)]
    |> dict.from_list
    |> dict.size
    == 2
}

pub fn from_list_duplicate_key_test() {
  assert dict.from_list([#(1, 0), #(1, 1)]) == dict.from_list([#(1, 1)])
}

pub fn from_list_inequality_value_test() {
  assert dict.from_list([#(1, 0), #(2, 1)])
    != dict.from_list([#(1, 0), #(2, 2)])
}

pub fn from_list_inequality_key_test() {
  assert dict.from_list([#(1, 0), #(2, 1)])
    != dict.from_list([#(1, 0), #(3, 1)])
}

pub fn has_key_empty_test() {
  assert !{
    []
    |> dict.from_list
    |> dict.has_key(1)
  }
}

pub fn has_key_present_test() {
  assert [#(1, 0)]
    |> dict.from_list
    |> dict.has_key(1)
}

pub fn has_key_present_multiple_test() {
  assert [#(4, 0), #(1, 0)]
    |> dict.from_list
    |> dict.has_key(1)
}

pub fn has_key_absent_test() {
  assert !{
    [#(4, 0), #(1, 0)]
    |> dict.from_list
    |> dict.has_key(0)
  }
}

pub fn new_size_test() {
  assert dict.size(dict.new()) == 0
}

pub fn new_to_list_test() {
  assert dict.to_list(dict.new()) == []
}

type Key {
  A
  B
  C
}

pub fn get_int_key_first_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, 4) == Ok(0)
}

pub fn get_int_key_second_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, 1) == Ok(1)
}

pub fn get_int_key_missing_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, 2) == Error(Nil)
}

pub fn get_custom_key_a_test() {
  let proplist = [#(A, 0), #(B, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, A) == Ok(0)
}

pub fn get_custom_key_b_test() {
  let proplist = [#(A, 0), #(B, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, B) == Ok(1)
}

pub fn get_custom_key_missing_test() {
  let proplist = [#(A, 0), #(B, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, C) == Error(Nil)
}

pub fn get_bit_array_key_first_test() {
  let proplist = [#(<<1, 2, 3>>, 0), #(<<3, 2, 1>>, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, <<1, 2, 3>>) == Ok(0)
}

pub fn get_bit_array_key_second_test() {
  let proplist = [#(<<1, 2, 3>>, 0), #(<<3, 2, 1>>, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, <<3, 2, 1>>) == Ok(1)
}

pub fn get_bit_array_key_missing_test() {
  let proplist = [#(<<1, 2, 3>>, 0), #(<<3, 2, 1>>, 1)]
  let m = dict.from_list(proplist)

  assert dict.get(m, <<1, 3, 2>>) == Error(Nil)
}

pub fn insert_test() {
  assert dict.new()
    |> dict.insert("a", 0)
    |> dict.insert("b", 1)
    |> dict.insert("c", 2)
    == dict.from_list([#("a", 0), #("b", 1), #("c", 2)])
}

pub fn map_values_test() {
  assert [#(1, 0), #(2, 1), #(3, 2)]
    |> dict.from_list
    |> dict.map_values(fn(k, v) { k + v })
    == dict.from_list([#(1, 1), #(2, 3), #(3, 5)])
}

pub fn keys_test() {
  assert [#("a", 0), #("b", 1), #("c", 2)]
    |> dict.from_list
    |> dict.keys
    |> list.sort(string.compare)
    == ["a", "b", "c"]
}

pub fn values_test() {
  assert [#("a", 0), #("b", 1), #("c", 2)]
    |> dict.from_list
    |> dict.values
    |> list.sort(int.compare)
    == [0, 1, 2]
}

pub fn take_test() {
  assert [#("a", 0), #("b", 1), #("c", 2)]
    |> dict.from_list
    |> dict.take(["a", "b", "d"])
    == dict.from_list([#("a", 0), #("b", 1)])
}

pub fn drop_test() {
  assert [#("a", 0), #("b", 1), #("c", 2)]
    |> dict.from_list
    |> dict.drop(["a", "b", "d"])
    == dict.from_list([#("c", 2)])
}

pub fn merge_same_key_first_wins_test() {
  let a = dict.from_list([#("a", 2)])
  let b = dict.from_list([#("a", 0)])

  assert dict.merge(a, b) == dict.from_list([#("a", 0)])
}

pub fn merge_same_key_second_wins_test() {
  let a = dict.from_list([#("a", 2)])
  let b = dict.from_list([#("a", 0)])

  assert dict.merge(b, a) == dict.from_list([#("a", 2)])
}

pub fn merge_first_test() {
  let a = dict.from_list([#("a", 2), #("c", 4), #("d", 3)])
  let b = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  assert dict.merge(a, b)
    == dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])
}

pub fn merge_second_test() {
  let a = dict.from_list([#("a", 2), #("c", 4), #("d", 3)])
  let b = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  assert dict.merge(b, a)
    == dict.from_list([#("a", 2), #("b", 1), #("c", 4), #("d", 3)])
}

pub fn delete_test() {
  assert [#("a", 0), #("b", 1), #("c", 2)]
    |> dict.from_list
    |> dict.delete("a")
    |> dict.delete("d")
    == dict.from_list([#("b", 1), #("c", 2)])
}

pub fn upsert_existing_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  assert dict.upsert(dict, "a", inc_or_zero)
    == dict.from_list([#("a", 1), #("b", 1), #("c", 2)])
}

pub fn upsert_existing_b_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  assert dict.upsert(dict, "b", inc_or_zero)
    == dict.from_list([#("a", 0), #("b", 2), #("c", 2)])
}

pub fn upsert_new_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  assert dict.upsert(dict, "z", inc_or_zero)
    == dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("z", 0)])
}

pub fn fold_sum_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])

  let add = fn(acc, _, v) { v + acc }

  assert dict.fold(dict, 0, add) == 6
}

pub fn fold_keys_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])

  let prepend = fn(acc, k, _) { list.prepend(acc, k) }

  assert dict
    |> dict.fold([], prepend)
    |> list.sort(string.compare)
    == ["a", "b", "c", "d"]
}

pub fn fold_empty_test() {
  let add = fn(acc, _, v) { v + acc }

  assert dict.fold(dict.from_list([]), 0, add) == 0
}

pub fn each_test() {
  let dict = dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)])

  assert dict.each(dict, fn(k, v) {
      let assert True = case k, v {
        "a", 1 | "b", 2 | "c", 3 | "d", 4 -> True
        _, _ -> False
      }
    })
    == Nil
}

fn range(start, end, a) {
  case end - start {
    n if n < 1 -> a
    _ -> range(start, end - 1, [end - 1, ..a])
  }
}

fn list_to_map(list) {
  list
  |> list.map(fn(n) { #(n, n) })
  |> dict.from_list
}

fn grow_and_shrink_map(initial_size, final_size) {
  range(0, initial_size, [])
  |> list_to_map
  |> list.fold(range(final_size, initial_size, []), _, fn(map, item) {
    dict.delete(map, item)
  })
}

// maps should be equal even if the insert/removal order was different
pub fn insert_order_equality_small_test() {
  assert grow_and_shrink_map(8, 2) == grow_and_shrink_map(4, 2)
}

pub fn insert_order_equality_medium_test() {
  assert grow_and_shrink_map(17, 10) == grow_and_shrink_map(12, 10)
}

pub fn insert_order_equality_large_test() {
  assert grow_and_shrink_map(2000, 1000) == grow_and_shrink_map(1000, 1000)
}

// ensure operations on a map don't mutate it
pub fn persistence_test() {
  let a = list_to_map([0])
  let _ = dict.insert(a, 0, 5)
  let _ = dict.insert(a, 1, 6)
  let _ = dict.delete(a, 0)
  assert dict.get(a, 0) == Ok(0)
}

// using maps as keys should work (tests hash function)
pub fn map_as_key_test() {
  let l = range(0, 1000, [])
  let a = list_to_map(l)
  let a2 = list_to_map(list.reverse(l))
  let a3 = grow_and_shrink_map(2000, 1000)
  let b = grow_and_shrink_map(60, 50)
  let c = grow_and_shrink_map(50, 20)
  let d = grow_and_shrink_map(2, 2)

  let map1 =
    dict.new()
    |> dict.insert(a, "a")
    |> dict.insert(b, "b")
    |> dict.insert(c, "c")
    |> dict.insert(d, "d")

  assert dict.get(map1, a) == Ok("a")
  assert dict.get(map1, a2) == Ok("a")
  assert dict.get(map1, a3) == Ok("a")
  assert dict.get(map1, b) == Ok("b")
  assert dict.get(map1, c) == Ok("c")
  assert dict.get(map1, d) == Ok("d")
  assert dict.get(dict.insert(map1, a2, "a2"), a) == Ok("a2")
  assert dict.get(dict.insert(map1, a3, "a3"), a) == Ok("a3")
}

pub fn large_n_get_test() {
  let n = 10_000
  let l = range(0, n, [])

  let m = list_to_map(l)
  list.map(l, fn(i) {
    assert dict.get(m, i) == Ok(i)
  })
  Nil
}

pub fn large_n_after_shrink_test() {
  let n = 10_000
  let l = range(0, n, [])

  let m = grow_and_shrink_map(n, 0)
  list.map(l, fn(i) {
    assert dict.get(m, i) == Error(Nil)
  })
  Nil
}

pub fn size_basic_test() {
  let n = 1000
  let m = list_to_map(range(0, n, []))
  assert dict.size(m) == n
}

pub fn size_after_shrink_test() {
  let n = 1000
  let m = grow_and_shrink_map(n, n / 2)
  assert dict.size(m) == n / 2
}

pub fn size_empty_after_delete_test() {
  let n = 1000
  let m =
    grow_and_shrink_map(n, 0)
    |> dict.delete(0)
  assert dict.size(m) == 0
}

pub fn size_insert_existing_test() {
  let m = list_to_map(range(0, 18, []))

  assert dict.size(dict.insert(m, 1, 99)) == 18
}

pub fn size_insert_existing_2_test() {
  let m = list_to_map(range(0, 18, []))

  assert dict.size(dict.insert(m, 2, 99)) == 18
}

pub fn is_empty_new_test() {
  assert dict.is_empty(dict.new())
}

pub fn is_empty_with_element_test() {
  assert !{
    dict.new()
    |> dict.insert(1, 10)
    |> dict.is_empty()
  }
}

pub fn is_empty_after_delete_test() {
  assert dict.new()
    |> dict.insert(1, 10)
    |> dict.delete(1)
    |> dict.is_empty()
}

// https://github.com/gleam-lang/stdlib/issues/435
pub fn peters_bug_test() {
  assert dict.new()
    |> dict.insert(22, Nil)
    |> dict.insert(21, Nil)
    |> dict.insert(23, Nil)
    |> dict.insert(18, Nil)
    |> dict.insert(17, Nil)
    |> dict.insert(19, Nil)
    |> dict.insert(14, Nil)
    |> dict.insert(13, Nil)
    |> dict.insert(15, Nil)
    |> dict.insert(10, Nil)
    |> dict.insert(9, Nil)
    |> dict.insert(11, Nil)
    |> dict.insert(6, Nil)
    |> dict.insert(5, Nil)
    |> dict.insert(7, Nil)
    |> dict.insert(2, Nil)
    |> dict.insert(1, Nil)
    |> dict.insert(3, Nil)
    |> dict.get(0)
    == Error(Nil)
}

pub fn zero_must_be_contained_get_test() {
  let map =
    dict.new()
    |> dict.insert(0, Nil)

  assert dict.get(map, 0) == Ok(Nil)
}

pub fn zero_must_be_contained_has_key_test() {
  let map =
    dict.new()
    |> dict.insert(0, Nil)

  assert dict.has_key(map, 0) == True
}

pub fn empty_map_equality_first_test() {
  let map1 = dict.new()
  let map2 = dict.from_list([#(1, 2)])

  assert map1 != map2
}

pub fn empty_map_equality_second_test() {
  let map1 = dict.new()
  let map2 = dict.from_list([#(1, 2)])

  assert map2 != map1
}

pub fn extra_keys_equality_first_test() {
  let map1 = dict.from_list([#(1, 2), #(3, 4)])
  let map2 = dict.from_list([#(1, 2), #(3, 4), #(4, 5)])

  assert map1 != map2
}

pub fn extra_keys_equality_second_test() {
  let map1 = dict.from_list([#(1, 2), #(3, 4)])
  let map2 = dict.from_list([#(1, 2), #(3, 4), #(4, 5)])

  assert map2 != map1
}

pub fn combine_test() {
  let map1 = dict.from_list([#("a", 3), #("b", 2)])
  let map2 = dict.from_list([#("a", 2), #("c", 3), #("d", 4)])

  assert dict.combine(map1, map2, fn(one, other) { one - other })
    == dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)])
}

pub fn combine_with_empty_first_test() {
  let map1 = dict.from_list([#("a", 3), #("b", 2)])

  assert dict.combine(map1, dict.new(), fn(one, _) { one }) == map1
}

pub fn combine_with_empty_second_test() {
  let map1 = dict.from_list([#("a", 3), #("b", 2)])

  assert dict.combine(dict.new(), map1, fn(one, _) { one }) == map1
}

pub fn combine_with_no_overlapping_keys_test() {
  let map1 = dict.from_list([#("a", 1), #("b", 2)])
  let map2 = dict.from_list([#("c", 3), #("d", 4)])

  assert dict.combine(map1, map2, fn(one, _) { one })
    == dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)])
}

// Enums without fields all hash to 0 due to how the hash function works -
// we use this fact here to produce and test collisions.
//
// Object.keys() returns [] for variants without fields, so the hash always
// stays on it's initial value.
type CollidingKey {
  CollidingKey1
  CollidingKey2
}

pub fn hash_collision_size_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)

  assert dict.size(d) == 2
}

pub fn hash_collision_get_first_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)

  assert dict.get(d, CollidingKey1) == Ok(1)
}

pub fn hash_collision_get_second_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)

  assert dict.get(d, CollidingKey2) == Ok(2)
}

pub fn hash_collision_after_delete_size_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)
  let d = dict.delete(d, CollidingKey1)

  assert dict.size(d) == 1
}

pub fn hash_collision_after_delete_get_first_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)
  let d = dict.delete(d, CollidingKey1)

  assert dict.get(d, CollidingKey1) == Error(Nil)
}

pub fn hash_collision_after_delete_get_second_test() {
  let d =
    dict.new() |> dict.insert(CollidingKey1, 1) |> dict.insert(CollidingKey2, 2)
  let d = dict.delete(d, CollidingKey1)

  assert dict.get(d, CollidingKey2) == Ok(2)
}

type Colour {
  Red
  Green
  Blue
}

pub fn hash_collision_fold_green_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let str =
    dict.fold(colours, "", fn(acc, _color, name) { acc <> name <> "\n" })
  assert string.contains(str, "green\n")
}

pub fn hash_collision_fold_red_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let str =
    dict.fold(colours, "", fn(acc, _color, name) { acc <> name <> "\n" })
  assert string.contains(str, "red\n")
}

pub fn hash_collision_fold_blue_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let str =
    dict.fold(colours, "", fn(acc, _color, name) { acc <> name <> "\n" })
  assert string.contains(str, "blue\n")
}

pub fn hash_collision_map_size_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let uppercase =
    dict.map_values(colours, fn(_colour, name) { string.uppercase(name) })

  assert dict.size(uppercase) == 3
}

pub fn hash_collision_map_red_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let uppercase =
    dict.map_values(colours, fn(_colour, name) { string.uppercase(name) })

  assert dict.get(uppercase, Red) == Ok("RED")
}

pub fn hash_collision_map_green_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let uppercase =
    dict.map_values(colours, fn(_colour, name) { string.uppercase(name) })

  assert dict.get(uppercase, Green) == Ok("GREEN")
}

pub fn hash_collision_map_blue_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  let uppercase =
    dict.map_values(colours, fn(_colour, name) { string.uppercase(name) })

  assert dict.get(uppercase, Blue) == Ok("BLUE")
}

pub fn hash_collision_values_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  assert list.sort(dict.values(colours), string.compare)
    == ["blue", "green", "red"]
}

pub fn hash_collision_keys_test() {
  let colours =
    dict.from_list([
      #(Red, "red"),
      #(Green, "green"),
      #(Blue, "blue"),
    ])

  assert dict.keys(colours)
    |> list.filter_map(dict.get(colours, _))
    |> list.sort(string.compare)
    == ["blue", "green", "red"]
}

fn test_random_operations(
  initial_seed: Int,
  num_ops: Int,
  key_space: Int,
  initial: dict.Dict(Int, Int),
) -> Nil {
  test_random_operations_loop(
    initial_seed,
    prng(initial_seed),
    num_ops,
    key_space,
    dict.to_list(initial),
    initial,
  )
}

fn test_random_operations_loop(
  initial_seed: Int,
  seed: Int,
  remaining: Int,
  key_space: Int,
  proplist: List(#(Int, Int)),
  dict: dict.Dict(Int, Int),
) -> Nil {
  case remaining > 0 {
    False -> {
      assert_dict_matches_proplist(dict, proplist, initial_seed)
    }
    True -> {
      let seed = prng(seed)
      let op_choice = seed % 2
      let seed = prng(seed)
      let key = seed % key_space

      case op_choice {
        // Insert
        0 -> {
          let new_proplist = list.key_set(proplist, key, key * 2)
          let new_dict = dict.insert(dict, key, key * 2)
          test_random_operations_loop(
            initial_seed,
            seed,
            remaining - 1,
            key_space,
            new_proplist,
            new_dict,
          )
        }
        // Delete
        _ -> {
          let new_proplist = case list.key_pop(proplist, key) {
            Ok(#(_, remaining)) -> remaining
            Error(Nil) -> proplist
          }
          let new_dict = dict.delete(dict, key)
          test_random_operations_loop(
            initial_seed,
            seed,
            remaining - 1,
            key_space,
            new_proplist,
            new_dict,
          )
        }
      }
    }
  }
}

fn run_many_random_tests(
  count count: Int,
  ops_per_test ops_per_test: Int,
  key_space key_space: Int,
  initial dict: dict.Dict(Int, Int),
) -> Nil {
  case count {
    0 -> Nil
    _ -> {
      let start_seed = int.random(0x7fffffff)
      test_random_operations(start_seed, ops_per_test, key_space, dict)
      run_many_random_tests(
        count: count - 1,
        ops_per_test: ops_per_test,
        key_space: key_space,
        initial: dict,
      )
    }
  }
}

pub fn random_operations_small_test() {
  run_many_random_tests(
    count: 100,
    ops_per_test: 50,
    key_space: 32,
    initial: dict.new(),
  )
}

pub fn random_operations_medium_test() {
  run_many_random_tests(
    count: 100,
    ops_per_test: 50,
    key_space: 200,
    initial: range_dict(50),
  )
}

pub fn random_operations_large_test() {
  run_many_random_tests(
    count: 100,
    ops_per_test: 1000,
    key_space: 2000,
    initial: range_dict(1000),
  )
}

fn range_dict(size) {
  int.range(from: size, to: 0, with: [], run: list.prepend)
  |> list.map(fn(x) { #(x, x) })
  |> dict.from_list
}

fn prng(state: Int) -> Int {
  { state * 48_271 } % 0x7FFFFFFF
}

fn assert_dict_matches_proplist(
  d: dict.Dict(k, v),
  proplist: List(#(k, v)),
  seed: Int,
) -> Nil {
  case dict.size(d) == list.length(proplist) {
    True -> Nil
    False ->
      panic as {
        "Size mismatch with seed "
        <> int.to_string(seed)
        <> ": dict.size="
        <> int.to_string(dict.size(d))
        <> " proplist.size="
        <> int.to_string(list.length(proplist))
      }
  }

  list.each(proplist, fn(pair) {
    let #(key, value) = pair
    let result = dict.get(d, key)

    case result == Ok(value) {
      True -> Nil
      False ->
        panic as {
          "Get mismatch with seed "
          <> int.to_string(seed)
          <> ": key="
          <> string.inspect(key)
          <> ", value="
          <> string.inspect(value)
          <> ", dict.get="
          <> string.inspect(result)
        }
    }
  })

  case d == dict.from_list(proplist) {
    True -> Nil
    False ->
      panic as {
        "Structural equality failed with seed " <> int.to_string(seed)
      }
  }
}

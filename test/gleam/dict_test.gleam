import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/should
import gleam/string

pub fn from_list_test() {
  [#(4, 0), #(1, 0)]
  |> dict.from_list
  |> dict.size
  |> should.equal(2)

  [#(1, 0), #(1, 1)]
  |> dict.from_list
  |> should.equal(dict.from_list([#(1, 1)]))

  [#(1, 0), #(2, 1)]
  |> dict.from_list
  |> should.not_equal(dict.from_list([#(1, 0), #(2, 2)]))

  [#(1, 0), #(2, 1)]
  |> dict.from_list
  |> should.not_equal(dict.from_list([#(1, 0), #(3, 1)]))
}

pub fn has_key_test() {
  []
  |> dict.from_list
  |> dict.has_key(1)
  |> should.be_false

  [#(1, 0)]
  |> dict.from_list
  |> dict.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> dict.from_list
  |> dict.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> dict.from_list
  |> dict.has_key(0)
  |> should.be_false
}

pub fn new_test() {
  dict.new()
  |> dict.size
  |> should.equal(0)

  dict.new()
  |> dict.to_list
  |> should.equal([])
}

type Key {
  A
  B
  C
}

pub fn get_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = dict.from_list(proplist)

  m
  |> dict.get(4)
  |> should.equal(Ok(0))

  m
  |> dict.get(1)
  |> should.equal(Ok(1))

  m
  |> dict.get(2)
  |> should.equal(Error(Nil))

  let proplist = [#(A, 0), #(B, 1)]
  let m = dict.from_list(proplist)

  m
  |> dict.get(A)
  |> should.equal(Ok(0))

  m
  |> dict.get(B)
  |> should.equal(Ok(1))

  m
  |> dict.get(C)
  |> should.equal(Error(Nil))

  let proplist = [#(<<1, 2, 3>>, 0), #(<<3, 2, 1>>, 1)]
  let m = dict.from_list(proplist)

  m
  |> dict.get(<<1, 2, 3>>)
  |> should.equal(Ok(0))

  m
  |> dict.get(<<3, 2, 1>>)
  |> should.equal(Ok(1))

  m
  |> dict.get(<<1, 3, 2>>)
  |> should.equal(Error(Nil))
}

pub fn insert_test() {
  dict.new()
  |> dict.insert("a", 0)
  |> dict.insert("b", 1)
  |> dict.insert("c", 2)
  |> should.equal(dict.from_list([#("a", 0), #("b", 1), #("c", 2)]))
}

pub fn map_values_test() {
  [#(1, 0), #(2, 1), #(3, 2)]
  |> dict.from_list
  |> dict.map_values(fn(k, v) { k + v })
  |> should.equal(dict.from_list([#(1, 1), #(2, 3), #(3, 5)]))
}

pub fn keys_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> dict.from_list
  |> dict.keys
  |> list.sort(string.compare)
  |> should.equal(["a", "b", "c"])
}

pub fn values_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> dict.from_list
  |> dict.values
  |> list.sort(int.compare)
  |> should.equal([0, 1, 2])
}

pub fn take_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> dict.from_list
  |> dict.take(["a", "b", "d"])
  |> should.equal(dict.from_list([#("a", 0), #("b", 1)]))
}

pub fn drop_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> dict.from_list
  |> dict.drop(["a", "b", "d"])
  |> should.equal(dict.from_list([#("c", 2)]))
}

pub fn merge_same_key_test() {
  let a = dict.from_list([#("a", 2)])
  let b = dict.from_list([#("a", 0)])

  dict.merge(a, b)
  |> should.equal(dict.from_list([#("a", 0)]))

  dict.merge(b, a)
  |> should.equal(dict.from_list([#("a", 2)]))
}

pub fn merge_test() {
  let a = dict.from_list([#("a", 2), #("c", 4), #("d", 3)])
  let b = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  dict.merge(a, b)
  |> should.equal(dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)]))

  dict.merge(b, a)
  |> should.equal(dict.from_list([#("a", 2), #("b", 1), #("c", 4), #("d", 3)]))
}

pub fn delete_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> dict.from_list
  |> dict.delete("a")
  |> dict.delete("d")
  |> should.equal(dict.from_list([#("b", 1), #("c", 2)]))
}

pub fn upsert_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  dict
  |> dict.upsert("a", inc_or_zero)
  |> should.equal(dict.from_list([#("a", 1), #("b", 1), #("c", 2)]))

  dict
  |> dict.upsert("b", inc_or_zero)
  |> should.equal(dict.from_list([#("a", 0), #("b", 2), #("c", 2)]))

  dict
  |> dict.upsert("z", inc_or_zero)
  |> should.equal(dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("z", 0)]))
}

pub fn fold_test() {
  let dict = dict.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])

  let add = fn(acc, _, v) { v + acc }

  dict
  |> dict.fold(0, add)
  |> should.equal(6)

  let prepend = fn(acc, k, _) { list.prepend(acc, k) }

  dict
  |> dict.fold([], prepend)
  |> list.sort(string.compare)
  |> should.equal(["a", "b", "c", "d"])

  dict.from_list([])
  |> dict.fold(0, add)
  |> should.equal(0)
}

pub fn each_test() {
  let dict = dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)])

  dict.each(dict, fn(k, v) {
    let assert True = case k, v {
      "a", 1 | "b", 2 | "c", 3 | "d", 4 -> True
      _, _ -> False
    }
  })
  |> should.equal(Nil)
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
pub fn insert_order_equality_test() {
  grow_and_shrink_map(8, 2)
  |> should.equal(grow_and_shrink_map(4, 2))
  grow_and_shrink_map(17, 10)
  |> should.equal(grow_and_shrink_map(12, 10))
  grow_and_shrink_map(2000, 1000)
  |> should.equal(grow_and_shrink_map(1000, 1000))
}

// ensure operations on a map don't mutate it
pub fn persistence_test() {
  let a = list_to_map([0])
  let _ = dict.insert(a, 0, 5)
  let _ = dict.insert(a, 1, 6)
  let _ = dict.delete(a, 0)
  dict.get(a, 0)
  |> should.equal(Ok(0))
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

  dict.get(map1, a)
  |> should.equal(Ok("a"))
  dict.get(map1, a2)
  |> should.equal(Ok("a"))
  dict.get(map1, a3)
  |> should.equal(Ok("a"))
  dict.get(map1, b)
  |> should.equal(Ok("b"))
  dict.get(map1, c)
  |> should.equal(Ok("c"))
  dict.get(map1, d)
  |> should.equal(Ok("d"))
  dict.insert(map1, a2, "a2")
  |> dict.get(a)
  |> should.equal(Ok("a2"))
  dict.insert(map1, a3, "a3")
  |> dict.get(a)
  |> should.equal(Ok("a3"))
}

pub fn large_n_test() {
  let n = 10_000
  let l = range(0, n, [])

  let m = list_to_map(l)
  list.map(l, fn(i) { should.equal(dict.get(m, i), Ok(i)) })

  let m = grow_and_shrink_map(n, 0)
  list.map(l, fn(i) { should.equal(dict.get(m, i), Error(Nil)) })
}

pub fn size_test() {
  let n = 1000
  let m = list_to_map(range(0, n, []))
  dict.size(m)
  |> should.equal(n)

  let m = grow_and_shrink_map(n, n / 2)
  dict.size(m)
  |> should.equal(n / 2)

  let m =
    grow_and_shrink_map(n, 0)
    |> dict.delete(0)
  dict.size(m)
  |> should.equal(0)

  let m = list_to_map(range(0, 18, []))

  dict.insert(m, 1, 99)
  |> dict.size()
  |> should.equal(18)
  dict.insert(m, 2, 99)
  |> dict.size()
  |> should.equal(18)
}

pub fn is_empty_test() {
  dict.new()
  |> dict.is_empty()
  |> should.be_true()

  dict.new()
  |> dict.insert(1, 10)
  |> dict.is_empty()
  |> should.be_false()

  dict.new()
  |> dict.insert(1, 10)
  |> dict.delete(1)
  |> dict.is_empty()
  |> should.be_true()
}

// https://github.com/gleam-lang/stdlib/issues/435
pub fn peters_bug_test() {
  dict.new()
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
  |> should.equal(Error(Nil))
}

pub fn zero_must_be_contained_test() {
  let map =
    dict.new()
    |> dict.insert(0, Nil)

  map
  |> dict.get(0)
  |> should.equal(Ok(Nil))

  map
  |> dict.has_key(0)
  |> should.equal(True)
}

pub fn empty_map_equality_test() {
  let map1 = dict.new()
  let map2 = dict.from_list([#(1, 2)])

  should.be_false(map1 == map2)
  should.be_false(map2 == map1)
}

pub fn extra_keys_equality_test() {
  let map1 = dict.from_list([#(1, 2), #(3, 4)])
  let map2 = dict.from_list([#(1, 2), #(3, 4), #(4, 5)])

  should.be_false(map1 == map2)
  should.be_false(map2 == map1)
}

pub fn combine_test() {
  let map1 = dict.from_list([#("a", 3), #("b", 2)])
  let map2 = dict.from_list([#("a", 2), #("c", 3), #("d", 4)])

  dict.combine(map1, map2, fn(one, other) { one - other })
  |> should.equal(dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)]))
}

pub fn combine_with_empty_test() {
  let map1 = dict.from_list([#("a", 3), #("b", 2)])

  dict.combine(map1, dict.new(), fn(one, _) { one })
  |> should.equal(map1)

  dict.combine(dict.new(), map1, fn(one, _) { one })
  |> should.equal(map1)
}

pub fn combine_with_no_overlapping_keys_test() {
  let map1 = dict.from_list([#("a", 1), #("b", 2)])
  let map2 = dict.from_list([#("c", 3), #("d", 4)])

  dict.combine(map1, map2, fn(one, _) { one })
  |> should.equal(dict.from_list([#("a", 1), #("b", 2), #("c", 3), #("d", 4)]))
}

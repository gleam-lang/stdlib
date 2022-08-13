import gleam/map
import gleam/option.{None, Some}
import gleam/should
import gleam/string
import gleam/list
import gleam/int

pub fn from_list_test() {
  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.size
  |> should.equal(2)

  [#(1, 0), #(1, 1)]
  |> map.from_list
  |> should.equal(map.from_list([#(1, 1)]))
}

pub fn has_key_test() {
  []
  |> map.from_list
  |> map.has_key(1)
  |> should.be_false

  [#(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.has_key(0)
  |> should.be_false
}

pub fn new_test() {
  map.new()
  |> map.size
  |> should.equal(0)

  map.new()
  |> map.to_list
  |> should.equal([])
}

type Key {
  A
  B
  C
}

pub fn get_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = map.from_list(proplist)

  m
  |> map.get(4)
  |> should.equal(Ok(0))

  m
  |> map.get(1)
  |> should.equal(Ok(1))

  m
  |> map.get(2)
  |> should.equal(Error(Nil))

  let proplist = [#(A, 0), #(B, 1)]
  let m = map.from_list(proplist)

  m
  |> map.get(A)
  |> should.equal(Ok(0))

  m
  |> map.get(B)
  |> should.equal(Ok(1))

  m
  |> map.get(C)
  |> should.equal(Error(Nil))

  let proplist = [#(<<1, 2, 3>>, 0), #(<<3, 2, 1>>, 1)]
  let m = map.from_list(proplist)

  m
  |> map.get(<<1, 2, 3>>)
  |> should.equal(Ok(0))

  m
  |> map.get(<<3, 2, 1>>)
  |> should.equal(Ok(1))

  m
  |> map.get(<<1, 3, 2>>)
  |> should.equal(Error(Nil))
}

pub fn insert_test() {
  map.new()
  |> map.insert("a", 0)
  |> map.insert("b", 1)
  |> map.insert("c", 2)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2)]))
}

pub fn map_values_test() {
  [#(1, 0), #(2, 1), #(3, 2)]
  |> map.from_list
  |> map.map_values(fn(k, v) { k + v })
  |> should.equal(map.from_list([#(1, 1), #(2, 3), #(3, 5)]))
}

pub fn keys_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.keys
  |> list.sort(string.compare)
  |> should.equal(["a", "b", "c"])
}

pub fn values_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.values
  |> list.sort(int.compare)
  |> should.equal([0, 1, 2])
}

pub fn take_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.take(["a", "b", "d"])
  |> should.equal(map.from_list([#("a", 0), #("b", 1)]))
}

pub fn drop_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.drop(["a", "b", "d"])
  |> should.equal(map.from_list([#("c", 2)]))
}

pub fn merge_same_key_test() {
  let a = map.from_list([#("a", 2)])
  let b = map.from_list([#("a", 0)])

  map.merge(a, b)
  |> should.equal(map.from_list([#("a", 0)]))

  map.merge(b, a)
  |> should.equal(map.from_list([#("a", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([#("a", 2), #("c", 4), #("d", 3)])
  let b = map.from_list([#("a", 0), #("b", 1), #("c", 2)])

  map.merge(a, b)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)]))

  map.merge(b, a)
  |> should.equal(map.from_list([#("a", 2), #("b", 1), #("c", 4), #("d", 3)]))
}

pub fn delete_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.delete("a")
  |> map.delete("d")
  |> should.equal(map.from_list([#("b", 1), #("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  dict
  |> map.update("a", inc_or_zero)
  |> should.equal(map.from_list([#("a", 1), #("b", 1), #("c", 2)]))

  dict
  |> map.update("b", inc_or_zero)
  |> should.equal(map.from_list([#("a", 0), #("b", 2), #("c", 2)]))

  dict
  |> map.update("z", inc_or_zero)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2), #("z", 0)]))
}

pub fn fold_test() {
  let dict = map.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])

  let add = fn(acc, _, v) { v + acc }

  dict
  |> map.fold(0, add)
  |> should.equal(6)

  let prepend = fn(acc, k, _) { list.prepend(acc, k) }

  dict
  |> map.fold([], prepend)
  |> list.sort(string.compare)
  |> should.equal(["a", "b", "c", "d"])

  map.from_list([])
  |> map.fold(0, add)
  |> should.equal(0)
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
  |> map.from_list
}

fn grow_and_shrink_map(initial_size, final_size) {
  range(0, initial_size, [])
  |> list_to_map
  |> list.fold(
    range(final_size, initial_size, []),
    _,
    fn(map, item) { map.delete(map, item) },
  )
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
  map.insert(a, 0, 5)
  map.insert(a, 1, 6)
  map.delete(a, 0)
  map.get(a, 0)
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
    map.new()
    |> map.insert(a, "a")
    |> map.insert(b, "b")
    |> map.insert(c, "c")
    |> map.insert(d, "d")

  map.get(map1, a)
  |> should.equal(Ok("a"))
  map.get(map1, a2)
  |> should.equal(Ok("a"))
  map.get(map1, a3)
  |> should.equal(Ok("a"))
  map.get(map1, b)
  |> should.equal(Ok("b"))
  map.get(map1, c)
  |> should.equal(Ok("c"))
  map.get(map1, d)
  |> should.equal(Ok("d"))
  map.insert(map1, a2, "a2")
  |> map.get(a)
  |> should.equal(Ok("a2"))
  map.insert(map1, a3, "a3")
  |> map.get(a)
  |> should.equal(Ok("a3"))
}

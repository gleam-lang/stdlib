import gleam/string
import gleam/should
import gleam/map

pub fn from_list_test() {
  [tuple(4, 0), tuple(1, 0)]
  |> map.from_list
  |> map.size
  |> should.equal(2)

  [tuple(1, 0), tuple(1, 1)]
  |> map.from_list
  |> should.equal(map.from_list([tuple(1, 1)]))
}

pub fn has_key_test() {
  []
  |> map.from_list
  |> map.has_key(1)
  |> should.be_false

  [tuple(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [tuple(4, 0), tuple(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [tuple(4, 0), tuple(1, 0)]
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

pub fn get_test() {
  let proplist = [tuple(4, 0), tuple(1, 1)]
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
}

pub fn insert_test() {
  map.new()
  |> map.insert("a", 0)
  |> map.insert("b", 1)
  |> map.insert("c", 2)
  |> should.equal(map.from_list([tuple("a", 0), tuple("b", 1), tuple("c", 2)]))
}

pub fn map_values_test() {
  [tuple(1, 0), tuple(2, 1), tuple(3, 2)]
  |> map.from_list
  |> map.map_values(fn(k, v) { k + v })
  |> should.equal(map.from_list([tuple(1, 1), tuple(2, 3), tuple(3, 5)]))
}

pub fn keys_test() {
  [tuple("a", 0), tuple("b", 1), tuple("c", 2)]
  |> map.from_list
  |> map.keys
  |> should.equal(["a", "b", "c"])
}

pub fn values_test() {
  [tuple("a", 0), tuple("b", 1), tuple("c", 2)]
  |> map.from_list
  |> map.values
  |> should.equal([0, 1, 2])
}

pub fn take_test() {
  [tuple("a", 0), tuple("b", 1), tuple("c", 2)]
  |> map.from_list
  |> map.take(["a", "b", "d"])
  |> should.equal(map.from_list([tuple("a", 0), tuple("b", 1)]))
}

pub fn drop_test() {
  [tuple("a", 0), tuple("b", 1), tuple("c", 2)]
  |> map.from_list
  |> map.drop(["a", "b", "d"])
  |> should.equal(map.from_list([tuple("c", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([tuple("a", 2), tuple("c", 4), tuple("d", 3)])

  let b = map.from_list([tuple("a", 0), tuple("b", 1), tuple("c", 2)])

  map.merge(a, b)
  |> should.equal(map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
    tuple("d", 3),
  ]))

  map.merge(b, a)
  |> should.equal(map.from_list([
    tuple("a", 2),
    tuple("b", 1),
    tuple("c", 4),
    tuple("d", 3),
  ]))
}

pub fn delete_test() {
  [tuple("a", 0), tuple("b", 1), tuple("c", 2)]
  |> map.from_list
  |> map.delete("a")
  |> map.delete("d")
  |> should.equal(map.from_list([tuple("b", 1), tuple("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([tuple("a", 0), tuple("b", 1), tuple("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Ok(i) -> i + 1
      Error(_) -> 0
    }
  }

  dict
  |> map.update("a", inc_or_zero)
  |> should.equal(map.from_list([tuple("a", 1), tuple("b", 1), tuple("c", 2)]))

  dict
  |> map.update("b", inc_or_zero)
  |> should.equal(map.from_list([tuple("a", 0), tuple("b", 2), tuple("c", 2)]))

  dict
  |> map.update("z", inc_or_zero)
  |> should.equal(map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
    tuple("z", 0),
  ]))
}

pub fn fold_test() {
  let dict =
    map.from_list([tuple("a", 0), tuple("b", 1), tuple("c", 2), tuple("d", 3)])

  let add = fn(_, v, acc) { v + acc }

  dict
  |> map.fold(0, add)
  |> should.equal(6)

  let concat = fn(k, _, acc) { string.append(acc, k) }

  dict
  |> map.fold("", concat)
  |> should.equal("abcd")

  map.from_list([])
  |> map.fold(0, add)
  |> should.equal(0)
}

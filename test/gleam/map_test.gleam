import gleam/string
import gleam/should
import gleam/map

pub fn from_list_test() {
  [
    tuple(4, 0),
    tuple(1, 0),
  ]
  |> map.from_list
  |> map.size
  |> should.equal(_, 2)

  [
    tuple(1, 0),
    tuple(1, 1),
  ]
  |> map.from_list
  |> should.equal(map.from_list([tuple(1, 1)]))
}

pub fn has_key_test() {
  []
  |> map.from_list
  |> map.has_key(_, 1)
  |> should.be_false

  [
      tuple(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> should.be_true

  [
      tuple(4, 0),
      tuple(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> should.be_true

  [
    tuple(4, 0),
    tuple(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 0)
  |> should.be_false
}

pub fn new_test() {
  map.new()
  |> map.size
  |> should.equal(_, 0)

  map.new()
  |> map.to_list
  |> should.equal(_, [])
}

pub fn get_test() {
  let proplist = [
    tuple(4, 0),
    tuple(1, 1),
  ]
  let m = map.from_list(proplist)

  m
  |> map.get(_, 4)
  |> should.equal(_, Ok(0))

  m
  |> map.get(_, 1)
  |> should.equal(_, Ok(1))

  m
  |> map.get(_, 2)
  |> should.equal(_, Error(Nil))
}

pub fn insert_test() {
  map.new()
  |> map.insert(_, "a", 0)
  |> map.insert(_, "b", 1)
  |> map.insert(_, "c", 2)
  |> should.equal(_, map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]))
}

pub fn map_values_test() {
  [
    tuple(1, 0),
    tuple(2, 1),
    tuple(3, 2),
  ]
  |> map.from_list
  |> map.map_values(_, fn(k, v) { k + v })
  |> should.equal(_, map.from_list([
    tuple(1, 1),
    tuple(2, 3),
    tuple(3, 5),
  ]))
}

pub fn keys_test() {
  [
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]
  |> map.from_list
  |> map.keys
  |> should.equal(_, ["a", "b", "c"])
}

pub fn values_test() {
  [
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]
  |> map.from_list
  |> map.values
  |> should.equal(_, [0, 1, 2])
}

pub fn take_test() {
  [
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]
  |> map.from_list
  |> map.take(_, ["a", "b", "d"])
  |> should.equal(_, map.from_list([tuple("a", 0), tuple("b", 1)]))
}

pub fn drop_test() {
  [
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]
  |> map.from_list
  |> map.drop(_, ["a", "b", "d"])
  |> should.equal(_, map.from_list([tuple("c", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([
    tuple("a", 2),
    tuple("c", 4),
    tuple("d", 3),
  ])
  let b = map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ])

  map.merge(a, b)
  |> should.equal(_, map.from_list([
      tuple("a", 0),
      tuple("b", 1),
      tuple("c", 2),
      tuple("d", 3),
    ]))

  map.merge(b, a)
  |> should.equal(_, map.from_list([
      tuple("a", 2),
      tuple("b", 1),
      tuple("c", 4),
      tuple("d", 3),
    ]))
}

pub fn delete_test() {
  [
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ]
  |> map.from_list
  |> map.delete(_, "a")
  |> map.delete(_, "d")
  |> should.equal(_, map.from_list([tuple("b", 1), tuple("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
  ])

  let inc_or_zero = fn(x) {
    case x {
      Ok(i) -> i + 1
      Error(_) -> 0
    }
  }

  dict
  |> map.update(_, "a", inc_or_zero)
  |> should.equal(_, map.from_list([
    tuple("a", 1),
    tuple("b", 1),
    tuple("c", 2),
  ]))

  dict
  |> map.update(_, "b", inc_or_zero)
  |> should.equal(_, map.from_list([
    tuple("a", 0),
    tuple("b", 2),
    tuple("c", 2),
  ]))

  dict
  |> map.update(_, "z", inc_or_zero)
  |> should.equal(_, map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
    tuple("z", 0),
  ]))
}

pub fn fold_test() {
  let dict = map.from_list([
    tuple("a", 0),
    tuple("b", 1),
    tuple("c", 2),
    tuple("d", 3),
  ])

  let add = fn(_, v, acc) {
    v + acc
  }

  dict
  |> map.fold(_, 0, add)
  |> should.equal(_, 6)

  let concat = fn(k, _, acc) {
    string.append(acc, k)
  }

  dict
  |> map.fold(_, "", concat)
  |> should.equal(_, "abcd")

  map.from_list([])
  |> map.fold(_, 0, add)
  |> should.equal(_, 0)
}

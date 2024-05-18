import gleam/int
import gleam/list
import gleam/set
import gleam/should

pub fn size_test() {
  set.new()
  |> set.size
  |> should.equal(0)

  set.new()
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)

  set.new()
  |> set.insert(1)
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)
}

pub fn contains_test() {
  set.new()
  |> set.insert(1)
  |> set.contains(this: 1)
  |> should.be_true

  set.new()
  |> set.contains(this: 1)
  |> should.be_false
}

pub fn delete_test() {
  set.new()
  |> set.insert(1)
  |> set.delete(1)
  |> set.contains(1)
  |> should.be_false
}

pub fn to_list_test() {
  set.new()
  |> set.insert(2)
  |> set.insert(3)
  |> set.insert(4)
  |> set.to_list
  |> list.sort(by: int.compare)
  |> should.equal([2, 3, 4])
}

pub fn from_list_test() {
  [1, 1, 2, 4, 3, 2]
  |> set.from_list
  |> set.to_list
  |> list.sort(by: int.compare)
  |> should.equal([1, 2, 3, 4])
}

pub fn fold_test() {
  [1, 3, 9]
  |> set.from_list
  |> set.fold(from: 0, with: fn(m, a) { m + a })
  |> should.equal(13)
}

pub fn filter_test() {
  [1, 4, 6, 3, 675, 44, 67]
  |> set.from_list()
  |> set.filter(keeping: int.is_even)
  |> set.to_list
  |> list.sort(int.compare)
  |> should.equal([4, 6, 44])
}

pub fn take_test() {
  [1, 2, 3]
  |> set.from_list
  |> set.take([1, 3, 5])
  |> should.equal(set.from_list([1, 3]))
}

pub fn drop_test() {
  ["a", "b", "c"]
  |> set.from_list
  |> set.drop(["a", "b", "d"])
  |> should.equal(set.from_list(["c"]))
}

pub fn union_test() {
  set.union(set.from_list([1, 2]), set.from_list([2, 3]))
  |> set.to_list
  |> list.sort(int.compare)
  |> should.equal([1, 2, 3])
}

pub fn intersection_test() {
  set.intersection(set.from_list([1, 2]), set.from_list([2, 3]))
  |> set.to_list
  |> should.equal([2])
}

pub fn difference_test() {
  set.difference(set.from_list([1, 2]), set.from_list([2, 3, 4]))
  |> set.to_list
  |> should.equal([1])
}

pub fn is_subset_test() {
  set.is_subset(set.from_list([1, 2, 3, 4]), set.from_list([2, 4]))
  |> should.be_false()

  set.is_subset(set.from_list([2, 4]), set.from_list([1, 2, 3, 4]))
  |> should.be_true()

  set.is_subset(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))
  |> should.be_false()

  set.is_subset(set.from_list([1, 2]), set.from_list([2, 3, 4]))
  |> should.be_false()
}

pub fn is_disjoint_test() {
  set.is_disjoint(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))
  |> should.be_true()

  set.is_disjoint(set.from_list([1, 2]), set.from_list([2, 3, 4]))
  |> should.be_false()
}

pub fn symmetric_difference_test() {
  set.symmetric_difference(set.from_list([1, 2, 3]), set.from_list([3, 4]))
  |> should.equal(set.from_list([1, 2, 4]))
}

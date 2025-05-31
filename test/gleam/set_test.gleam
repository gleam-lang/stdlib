import gleam/int
import gleam/list
import gleam/set

pub fn size_test() {
  assert set.new()
    |> set.size
    == 0

  assert set.new()
    |> set.insert(1)
    |> set.insert(2)
    |> set.size
    == 2

  assert set.new()
    |> set.insert(1)
    |> set.insert(1)
    |> set.insert(2)
    |> set.size
    == 2
}

pub fn is_empty_test() {
  assert set.new()
    |> set.is_empty()

  assert !{
    set.new()
    |> set.insert(1)
    |> set.is_empty()
  }

  assert set.new()
    |> set.insert(1)
    |> set.delete(1)
    |> set.is_empty()
}

pub fn contains_test() {
  assert set.new()
    |> set.insert(1)
    |> set.contains(this: 1)

  assert !{
    set.new()
    |> set.contains(this: 1)
  }
}

pub fn delete_test() {
  assert !{
    set.new()
    |> set.insert(1)
    |> set.delete(1)
    |> set.contains(1)
  }
}

pub fn to_list_test() {
  assert set.new()
    |> set.insert(2)
    |> set.insert(3)
    |> set.insert(4)
    |> set.to_list
    |> list.sort(by: int.compare)
    == [2, 3, 4]
}

pub fn from_list_test() {
  assert [1, 1, 2, 4, 3, 2]
    |> set.from_list
    |> set.to_list
    |> list.sort(by: int.compare)
    == [1, 2, 3, 4]
}

pub fn fold_test() {
  assert [1, 3, 9]
    |> set.from_list
    |> set.fold(from: 0, with: fn(m, a) { m + a })
    == 13
}

pub fn map_test() {
  assert [1, 2, 3, 4]
    |> set.from_list
    |> set.map(with: int.to_string)
    == set.from_list(["1", "2", "3", "4"])
}

pub fn filter_test() {
  assert [1, 4, 6, 3, 675, 44, 67]
    |> set.from_list()
    |> set.filter(keeping: int.is_even)
    |> set.to_list
    |> list.sort(int.compare)
    == [4, 6, 44]
}

pub fn take_test() {
  assert [1, 2, 3]
    |> set.from_list
    |> set.take([1, 3, 5])
    == set.from_list([1, 3])
}

pub fn drop_test() {
  assert ["a", "b", "c"]
    |> set.from_list
    |> set.drop(["a", "b", "d"])
    == set.from_list(["c"])
}

pub fn union_test() {
  assert set.union(set.from_list([1, 2]), set.from_list([2, 3]))
    |> set.to_list
    |> list.sort(int.compare)
    == [1, 2, 3]
}

pub fn intersection_test() {
  assert set.intersection(set.from_list([1, 2]), set.from_list([2, 3]))
    |> set.to_list
    == [2]
}

pub fn difference_test() {
  assert set.difference(set.from_list([1, 2]), set.from_list([2, 3, 4]))
    |> set.to_list
    == [1]
}

pub fn is_subset_test() {
  assert !set.is_subset(set.from_list([1, 2, 3, 4]), set.from_list([2, 4]))

  assert set.is_subset(set.from_list([2, 4]), set.from_list([1, 2, 3, 4]))

  assert !set.is_subset(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))

  assert !set.is_subset(set.from_list([1, 2]), set.from_list([2, 3, 4]))
}

pub fn is_disjoint_test() {
  assert set.is_disjoint(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))

  assert !set.is_disjoint(set.from_list([1, 2]), set.from_list([2, 3, 4]))
}

pub fn symmetric_difference_test() {
  assert set.symmetric_difference(
      set.from_list([1, 2, 3]),
      set.from_list([3, 4]),
    )
    == set.from_list([1, 2, 4])
}

pub fn each_test() {
  assert [1, 2, 3]
    |> set.from_list
    |> set.each(fn(member) {
      case member {
        1 | 2 | 3 -> Nil
        _ -> panic as "unexpected value"
      }
    })
    == Nil
}

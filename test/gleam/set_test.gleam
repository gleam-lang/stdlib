import gleam/int
import gleam/list
import gleam/set

pub fn size_empty_test() {
  assert set.size(set.new()) == 0
}

pub fn size_with_elements_test() {
  assert set.new()
    |> set.insert(1)
    |> set.insert(2)
    |> set.size
    == 2
}

pub fn size_with_duplicates_test() {
  assert set.new()
    |> set.insert(1)
    |> set.insert(1)
    |> set.insert(2)
    |> set.size
    == 2
}

pub fn is_empty_new_test() {
  assert set.is_empty(set.new())
}

pub fn is_empty_with_element_test() {
  assert !{
    set.new()
    |> set.insert(1)
    |> set.is_empty()
  }
}

pub fn is_empty_after_delete_test() {
  assert set.new()
    |> set.insert(1)
    |> set.delete(1)
    |> set.is_empty()
}

pub fn contains_present_test() {
  assert set.new()
    |> set.insert(1)
    |> set.contains(this: 1)
}

pub fn contains_absent_test() {
  assert !set.contains(set.new(), this: 1)
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
  assert set.to_list(set.intersection(
      set.from_list([1, 2]),
      set.from_list([2, 3]),
    ))
    == [2]
}

pub fn difference_test() {
  assert set.to_list(set.difference(
      set.from_list([1, 2]),
      set.from_list([2, 3, 4]),
    ))
    == [1]
}

pub fn is_subset_false_superset_test() {
  assert !set.is_subset(set.from_list([1, 2, 3, 4]), set.from_list([2, 4]))
}

pub fn is_subset_true_test() {
  assert set.is_subset(set.from_list([2, 4]), set.from_list([1, 2, 3, 4]))
}

pub fn is_subset_disjoint_test() {
  assert !set.is_subset(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))
}

pub fn is_subset_partial_overlap_test() {
  assert !set.is_subset(set.from_list([1, 2]), set.from_list([2, 3, 4]))
}

pub fn is_disjoint_true_test() {
  assert set.is_disjoint(set.from_list([1, 2, 3]), set.from_list([4, 5, 6]))
}

pub fn is_disjoint_false_test() {
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

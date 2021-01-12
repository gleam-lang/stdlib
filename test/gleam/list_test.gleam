import gleam/should
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/pair

pub fn length_test() {
  list.length([])
  |> should.equal(0)

  list.length([1])
  |> should.equal(1)

  list.length([1, 1])
  |> should.equal(2)

  list.length([1, 1, 1])
  |> should.equal(3)
}

pub fn reverse_test() {
  list.reverse([])
  |> should.equal([])
  list.reverse([1, 2, 3, 4, 5])
  |> should.equal([5, 4, 3, 2, 1])
}

pub fn is_empty_test() {
  list.is_empty([])
  |> should.be_true
  list.is_empty([1])
  |> should.be_false
}

pub fn contains_test() {
  list.contains([0, 4, 5, 1], 1)
  |> should.be_true
  list.contains([0, 4, 5, 7], 1)
  |> should.be_false
  list.contains([], 1)
  |> should.be_false
}

pub fn head_test() {
  list.head([0, 4, 5, 7])
  |> should.equal(Ok(0))

  list.head([])
  |> should.equal(Error(Nil))
}

pub fn tail_test() {
  list.tail([0, 4, 5, 7])
  |> should.equal(Ok([4, 5, 7]))

  list.tail([0])
  |> should.equal(Ok([]))

  list.tail([])
  |> should.equal(Error(Nil))
}

pub fn filter_test() {
  []
  |> list.filter(fn(_) { True })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(_) { True })
  |> should.equal([0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x > 4 })
  |> should.equal([5, 7])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x < 4 })
  |> should.equal([0, 3])
}

pub fn filter_map_test() {
  [2, 4, 6, 1]
  |> list.filter_map(fn(x) { Ok(x + 1) })
  |> should.equal([3, 5, 7, 2])

  [2, 4, 6, 1]
  |> list.filter_map(Error)
  |> should.equal([])
}

pub fn map_test() {
  []
  |> list.map(fn(x) { x * 2 })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.map(fn(x) { x * 2 })
  |> should.equal([0, 8, 10, 14, 6])
}

pub fn try_map_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  [5, 6, 5, 6]
  |> list.try_map(fun)
  |> should.equal(Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
  |> list.try_map(fun)
  |> should.equal(Error(7))
}

pub fn drop_test() {
  []
  |> list.drop(5)
  |> should.equal([])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.drop(5)
  |> should.equal([6, 7, 8])
}

pub fn take_test() {
  []
  |> list.take(5)
  |> should.equal([])
  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.take(5)
  |> should.equal([1, 2, 3, 4, 5])
}

pub fn new_test() {
  list.new()
  |> should.equal([])
}

pub fn append_test() {
  list.append([1], [2, 3])
  |> should.equal([1, 2, 3])
}

pub fn flatten_test() {
  list.flatten([])
  |> should.equal([])

  list.flatten([[]])
  |> should.equal([])

  list.flatten([[], [], []])
  |> should.equal([])

  list.flatten([[1, 2], [], [3, 4]])
  |> should.equal([1, 2, 3, 4])
}

pub fn fold_test() {
  [1, 2, 3]
  |> list.fold([], fn(x, acc) { [x, ..acc] })
  |> should.equal([3, 2, 1])
}

pub fn fold_right_test() {
  [1, 2, 3]
  |> list.fold_right(from: [], with: fn(x, acc) { [x, ..acc] })
  |> should.equal([1, 2, 3])
}

pub fn index_fold_test() {
  ["a", "b", "c"]
  |> list.index_fold([], fn(ix, i, acc) { [tuple(ix, i), ..acc] })
  |> should.equal([tuple(2, "c"), tuple(1, "b"), tuple(0, "a")])
}

pub fn try_fold_test() {
  [1, 2, 3]
  |> list.try_fold(
    0,
    fn(i, acc) {
      case i < 4 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    },
  )
  |> should.equal(Ok(6))

  [1, 2, 3]
  |> list.try_fold(
    0,
    fn(i, acc) {
      case i < 3 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    },
  )
  |> should.equal(Error(Nil))
}

pub fn find_map_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  [1, 2, 3]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3, 2]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3]
  |> list.find_map(with: f)
  |> should.equal(Error(Nil))
}

pub fn find_test() {
  let is_two = fn(x) { x == 2 }

  [1, 2, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3, 2]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Error(Nil))
}

pub fn all_test() {
  list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })
  |> should.equal(True)

  list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.equal(False)

  list.all([], fn(_) { False })
  |> should.equal(True)
}

pub fn any_test() {
  list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })
  |> should.equal(True)

  list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.equal(False)

  list.any([], fn(_) { False })
  |> should.equal(False)
}

pub fn zip_test() {
  list.zip([], [1, 2, 3])
  |> should.equal([])

  list.zip([1, 2], [])
  |> should.equal([])

  list.zip([1, 2, 3], [4, 5, 6])
  |> should.equal([tuple(1, 4), tuple(2, 5), tuple(3, 6)])

  list.zip([5, 6], [1, 2, 3])
  |> should.equal([tuple(5, 1), tuple(6, 2)])

  list.zip([5, 6, 7], [1, 2])
  |> should.equal([tuple(5, 1), tuple(6, 2)])
}

pub fn strict_zip_test() {
  list.strict_zip([], [1, 2, 3])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([1, 2], [])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([1, 2, 3], [4, 5, 6])
  |> should.equal(Ok([tuple(1, 4), tuple(2, 5), tuple(3, 6)]))

  list.strict_zip([5, 6], [1, 2, 3])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([5, 6, 7], [1, 2])
  |> should.equal(Error(list.LengthMismatch))
}

pub fn unzip_test() {
  list.unzip([tuple(1, 2), tuple(3, 4)])
  |> should.equal(tuple([1, 3], [2, 4]))

  list.unzip([])
  |> should.equal(tuple([], []))
}

pub fn intersperse_test() {
  list.intersperse([1, 2, 3], 4)
  |> should.equal([1, 4, 2, 4, 3])

  list.intersperse([], 2)
  |> should.equal([])
}

pub fn at_test() {
  list.at([1, 2, 3], 2)
  |> should.equal(Ok(3))

  list.at([1, 2, 3], 5)
  |> should.equal(Error(Nil))

  list.at([], 0)
  |> should.equal(Error(Nil))

  list.at([1, 2, 3, 4, 5, 6], -1)
  |> should.equal(Error(Nil))
}

pub fn unique_test() {
  list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6])
  |> should.equal([1, 2, 3, 4, 5, 6])

  list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5])
  |> should.equal([7, 1, 45, 6, 2, 47, 5])

  list.unique([3, 4, 5])
  |> should.equal([3, 4, 5])

  list.unique([])
  |> should.equal([])
}

pub fn sort_test() {
  [4, 3, 6, 5, 4]
  |> list.sort(int.compare)
  |> should.equal([3, 4, 4, 5, 6])

  [4, 3, 6, 5, 4, 1]
  |> list.sort(int.compare)
  |> should.equal([1, 3, 4, 4, 5, 6])

  [4.1, 3.1, 6.1, 5.1, 4.1]
  |> list.sort(float.compare)
  |> should.equal([3.1, 4.1, 4.1, 5.1, 6.1])

  []
  |> list.sort(int.compare)
  |> should.equal([])
}

pub fn index_map_test() {
  list.index_map([3, 4, 5], fn(i, x) { tuple(i, x) })
  |> should.equal([tuple(0, 3), tuple(1, 4), tuple(2, 5)])

  let f = fn(i, x) { string.append(x, int.to_string(i)) }
  list.index_map(["a", "b", "c"], f)
  |> should.equal(["a0", "b1", "c2"])
}

pub fn range_test() {
  list.range(0, 0)
  |> should.equal([])

  list.range(1, 1)
  |> should.equal([])

  list.range(-1, -1)
  |> should.equal([])

  list.range(0, 1)
  |> should.equal([0])

  list.range(0, 5)
  |> should.equal([0, 1, 2, 3, 4])

  list.range(1, -5)
  |> should.equal([1, 0, -1, -2, -3, -4])
}

pub fn repeat_test() {
  list.repeat(1, -10)
  |> should.equal([])

  list.repeat(1, 0)
  |> should.equal([])

  list.repeat(2, 3)
  |> should.equal([2, 2, 2])

  list.repeat("x", 5)
  |> should.equal(["x", "x", "x", "x", "x"])
}

pub fn split_test() {
  []
  |> list.split(0)
  |> should.equal(tuple([], []))

  [0, 1, 2, 3, 4]
  |> list.split(0)
  |> should.equal(tuple([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(-2)
  |> should.equal(tuple([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(1)
  |> should.equal(tuple([0], [1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(3)
  |> should.equal(tuple([0, 1, 2], [3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(9)
  |> should.equal(tuple([0, 1, 2, 3, 4], []))
}

pub fn split_while_test() {
  []
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(tuple([], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(tuple([1, 2, 3, 4, 5], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x == 2 })
  |> should.equal(tuple([], [1, 2, 3, 4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 3 })
  |> should.equal(tuple([1, 2, 3], [4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= -3 })
  |> should.equal(tuple([], [1, 2, 3, 4, 5]))
}

pub fn key_find_test() {
  let proplist = [tuple(0, "1"), tuple(1, "2")]

  proplist
  |> list.key_find(0)
  |> should.equal(Ok("1"))

  proplist
  |> list.key_find(1)
  |> should.equal(Ok("2"))

  proplist
  |> list.key_find(2)
  |> should.equal(Error(Nil))
}

pub fn pop_test() {
  list.pop([1, 2, 3], fn(x) { x > 2 })
  |> should.equal(Ok(tuple(3, [1, 2])))

  list.pop([1, 2, 3], fn(x) { x > 4 })
  |> should.equal(Error(Nil))

  list.pop([], fn(_x) { True })
  |> should.equal(Error(Nil))
}

pub fn pop_map_test() {
  list.pop_map(["foo", "2", "3"], int.parse)
  |> should.equal(Ok(tuple(2, ["foo", "3"])))

  list.pop_map(["foo", "bar"], int.parse)
  |> should.equal(Error(Nil))

  list.pop_map([], int.parse)
  |> should.equal(Error(Nil))
}

pub fn key_pop_test() {
  list.key_pop([tuple("a", 0), tuple("b", 1)], "a")
  |> should.equal(Ok(tuple(0, [tuple("b", 1)])))

  list.key_pop([tuple("a", 0), tuple("b", 1)], "b")
  |> should.equal(Ok(tuple(1, [tuple("a", 0)])))

  list.key_pop([tuple("a", 0), tuple("b", 1)], "c")
  |> should.equal(Error(Nil))
}

pub fn key_set_test() {
  [tuple(5, 0), tuple(4, 1)]
  |> list.key_set(4, 100)
  |> should.equal([tuple(5, 0), tuple(4, 100)])

  [tuple(5, 0), tuple(4, 1)]
  |> list.key_set(1, 100)
  |> should.equal([tuple(5, 0), tuple(4, 1), tuple(1, 100)])
}

pub fn partition_test() {
  [1, 2, 3, 4, 5, 6, 7]
  |> list.partition(int.is_odd)
  |> should.equal(tuple([1, 3, 5, 7], [2, 4, 6]))
}

pub fn permutations_test() {
  [1, 2]
  |> list.permutations
  |> should.equal([[1, 2], [2, 1]])

  let expected = [
    [1, 2, 3],
    [1, 3, 2],
    [2, 1, 3],
    [2, 3, 1],
    [3, 1, 2],
    [3, 2, 1],
  ]

  [1, 2, 3]
  |> list.permutations
  |> should.equal(expected)

  ["a", "b"]
  |> list.permutations
  |> should.equal([["a", "b"], ["b", "a"]])
}

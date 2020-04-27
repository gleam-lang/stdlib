//// Lists are an ordered sequence of elements and are one of the most common
//// data types in Gleam.
////
//// New elements can be added and removed from the front of a list in
//// constant time, while adding and removing from the end requires traversing
//// the copying the whole list, so keep this in mind when designing your
//// programs.
////
//// There is a dedicated syntax for prefixing to a list:
////
////    let new_list = [1, 2, ..existing_list]
////
//// And a matching syntax for getting the first elements of a list:
////
////    case list {
////      [first_element, ..rest] -> first_element
////      _ -> "this pattern matches when the list is empty"
////    }
////

import gleam/int
import gleam/pair
import gleam/order.{Order}
import gleam/result.{Option}

pub type List(elements) =
  List(elements)

/// An error value returned by the `strict_zip` function.
///
pub type LengthMismatch {
  LengthMismatch
}

/// Count the number of elements in a given list.
///
/// This function has to traverse the list to determine the number of elements,
/// so it runs in linear time.
///
/// This function is natively implemented by the virtual machine and is highly
/// optimised.
///
/// ## Examples
///
///    > length([])
///    0
///
///    > length([1])
///    1
///
///    > length([1, 2])
///    2
///
///
pub external fn length(of: List(a)) -> Int = "erlang" "length"

/// Create a new list from a given list containing the same elements but in the
/// opposite order.
///
/// This function has to traverse the list to create the new reversed list, so
/// it runs in linear time.
///
/// This function is natively implemented by the virtual machine and is highly
/// optimised.
///
/// ## Examples
///
///    > reverse([])
///    []
///
///    > reverse([1])
///    [1]
///
///    > reverse([1, 2])
///    [2, 1]
///
///
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

/// Determine whether or not the list is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > is_empty([])
///    True
///
///    > is_empty([1])
///    False
///
///    > is_empty([1, 1])
///    False
///
///
pub fn is_empty(list: List(a)) -> Bool {
  list == []
}

/// Determine whether or not a given element exists within a given list.
///
/// This function traverses the list to find the element, so it runs in linear
/// time.
///
/// ## Examples
///
///    > contains([], 0)
///    True
///
///    > contains([0], 0)
///    True
///
///    > contains([1], 0)
///    False
///
///    > contains([1, 1], 0)
///    False
///
///    > contains([1, 0], 0)
///    True
///
///
pub fn contains(list: List(a), has elem: a) -> Bool {
  case list {
    [] -> False
    [head | rest] -> head == elem || contains(rest, elem)
  }
}

/// Get the first element from the start of the list, if there is one.
///
/// ## Examples
///
///    > head([])
///    Error(Nil)
///
///    > head([0])
///    Ok(0)
///
///    > head([1, 2])
///    Ok(1)
///
///
pub fn head(list: List(a)) -> Option(a) {
  case list {
    [] -> result.none()
    [x | _] -> Ok(x)
  }
}

/// Get the list minus the first element. If the list is empty `Error(Nil)` is
/// returned.
///
/// This function runs in constant time and does not make a copy of the list.
///
/// ## Examples
///
///    > tail([])
///    Error(Nil)
///
///    > tail([0])
///    Ok([])
///
///    > tail([1, 2])
///    Ok([2])
///
///
pub fn tail(list: List(a)) -> Option(List(a)) {
  case list {
    [] -> result.none()
    [_ | xs] -> Ok(xs)
  }
}

fn do_filter(list: List(a), fun: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [x | xs] -> {
      let new_acc = case fun(x) {
        True -> [x | acc]
        False -> acc
      }
      do_filter(xs, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `True`.
///
/// ## Examples
///
///    > filter([2, 4, 6, 1], fn(x) { x > 2 })
///    [4, 6]
///
///    > filter([2, 4, 6, 1], fn(x) { x > 6 })
///    []
///
///
pub fn filter(list: List(a), for predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}

fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x | xs] -> do_map(xs, fun, [fun(x) | acc])
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one.
///
/// ## Examples
///
///    > map([2, 4, 6], fn(x) { x * 2 })
///    [4, 8, 12]
///
///
pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  do_map(list, fun, [])
}

fn do_index_map(
  list: List(a),
  fun: fn(Int, a) -> b,
  index: Int,
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x | xs] -> do_index_map(xs, fun, index + 1, [fun(index, x) | acc])
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one and their index.
///
/// The index starts at 0, so the first element is 0, the second is 1, and so
/// on.
///
/// ## Examples
///
///    > index_map(["a", "b"], fn(i, x) { tuple(i, x) })
///    [tuple(0, "a"), tuple(1, "b")]
///
///
pub fn index_map(list: List(a), with fun: fn(Int, a) -> b) -> List(b) {
  do_index_map(list, fun, 0, [])
}


fn do_traverse(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(reverse(acc))
    [x | xs] ->
      case fun(x) {
        Ok(y) -> do_traverse(xs, fun, [y | acc])
        Error(error) -> Error(error)
      }
  }
}

/// Takes a function that returns a Result applies it to each element in a
/// given list in tern.
///
/// If the function returns `Ok(new_value)` for all elements in the list then a
/// list of the new values is returned.
///
/// If the function returns `Error(reason)` for any of the elements then it is
/// returned immediately. None of the elements in the list are processed after
/// one returns an `Error`.
///
/// ## Examples
///
///    > traverse([1, 2, 3], fn(x) { Ok(x + 2) })
///    Ok([3, 4, 5])
///
///    > traverse([1, 2, 3], fn(x) { Error(0) })
///    Error(0)
///
///    > traverse([[1], [2, 3]], head)
///    Ok([1, 2])
///
///    > traverse([[1], [], [2]], head)
///    Error(Nil)
///
///
pub fn traverse(
  list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  do_traverse(list, fun, [])
}

/// Returns a list that is the given list with up to the given number of
/// elements removed from the front of the list.
///
/// If the element has less than the number of elements an empty list is
/// returned.
///
/// This function runs in linear time but does not copy the list.
///
/// ## Examples
///
///    > drop([1, 2, 3, 4], 2)
///    [3, 4]
///
///    > drop([1, 2, 3, 4], 9)
///    []
///
///
pub fn drop(from list: List(a), up_to n: Int) -> List(a) {
  case n <= 0 {
    True -> list
    False ->
      case list {
        [] -> []
        [_ | xs] -> drop(xs, n - 1)
      }
  }
}

fn do_take(list: List(a), n: Int, acc: List(a)) -> List(a) {
  case n <= 0 {
    True -> reverse(acc)
    False ->
      case list {
        [] -> reverse(acc)
        [x | xs] -> do_take(xs, n - 1, [x | acc])
      }
  }
}

/// Returns a list containing the first given number of elements from the given
/// list.
///
/// If the element has less than the number of elements then the full list is
/// returned.
///
/// This function runs in linear time but does not copy the list.
///
/// ## Examples
///
///    > take([1, 2, 3, 4], 2)
///    [1, 2]
///
///    > take([1, 2, 3, 4], 9)
///    [1, 2, 3, 4]
///
///
pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  do_take(list, n, [])
}

/// Returns a new empty list.
///
/// ## Examples
///
///    > new()
///    []
///
///
pub fn new() -> List(a) {
  []
}

/// Join one list onto the end of another.
///
/// This function runs in linear time, and it traverses and copies the first
/// list.
///
/// ## Examples
///
///    > append([1, 2], [3])
///    [1, 2, 3]
///
///
pub external fn append(List(a), List(a)) -> List(a)
  = "lists" "append";

fn do_flatten(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> acc
    [l | rest] -> do_flatten(rest, append(acc, l))
  }
}

/// Flattens a list of lists into a single list.
///
/// This function runs in linear time, and it traverses and copies all the
/// inner lists.
///
/// ## Examples
///
///    > flatten([[1], [2, 3], []])
///    [1, 2, 3]
///
///
pub fn flatten(lists: List(List(a))) -> List(a) {
  do_flatten(lists, [])
}

/// Reduce a list of elements into a single value by calling a given function
/// on each element, going from left to right.
///
/// `fold([1, 2, 3], 0, add)` is the equivalent of `add(3, add(2, add(1, 0)))`.
///
/// This function runs in linear time.
///
pub fn fold(list: List(a), from initial: b, with fun: fn(a, b) -> b) -> b {
  case list {
    [] -> initial
    [x | rest] -> fold(rest, fun(x, initial), fun)
  }
}

/// Reduce a list of elements into a single value by calling a given function
/// on each element, going from right to left.
///
/// `fold_right([1, 2, 3], 0, add)` is the equivalent of
/// `add(1, add(2, add(3, 0)))`.
///
/// This function runs in linear time.
///
/// Unlike `fold` this function is not tail recursive. Where possible use
/// `fold` instead as it will use less memory.
///
pub fn fold_right(
  list: List(a),
  from initial: b,
  with fun: fn(a, b) -> b,
) -> b {
  case list {
    [] -> initial
    [x | rest] -> fun(x, fold_right(rest, initial, fun))
  }
}

/// Find the first element in a given list for which the given function returns
/// True.
///
/// Returns `Error(Nil)` if no the function does not return True for any of the
/// elements.
///
/// ## Examples
///
///    > find([1, 2, 3], fn(x) { x > 2 })
///    Ok(3)
///
///    > find([1, 2, 3], fn(x) { x > 4 })
///    Error(Nil)
///
///    > find([], fn(x) { True })
///    Error(Nil)
///
///
pub fn find(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Option(a) {
  case haystack {
    [] -> result.none()
    [x | rest] ->
      case is_desired(x) {
        True -> Ok(x)
        _ -> find(in: rest, one_that: is_desired)
      }
  }
}

/// Find the first element in a given list for which the given function returns
/// `Ok(new_value)` and return the new value for that element.
///
/// Returns `Error(Nil)` if no the function does not return Ok for any of the
/// elements.
///
/// ## Examples
///
///    > find_map([[], [2], [3]], head)
///    Ok(2)
///
///    > find_map([[], []], head)
///    Error(Nil)
///
///    > find_map([], head)
///    Error(Nil)
///
///
pub fn find_map(
  in haystack: List(a),
  with fun: fn(a) -> Option(b),
) -> Option(b) {
  case haystack {
    [] -> result.none()
    [x | rest] ->
      case fun(x) {
        Ok(x) -> Ok(x)
        _ -> find_map(in: rest, with: fun)
      }
  }
}

/// Returns True if the given function returns True for all the elements in
/// the given list. If the function returns False for any of the elements it
/// immediately returns False without checking the rest of the list.
///
/// ## Examples
///
///    > all([], fn(x) { x > 3 })
///    True
///
///    > all([4, 5], fn(x) { x > 3 })
///    True
///
///    > all([4, 3], fn(x) { x > 3 })
///    False
///
///
pub fn all(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [x | rest] ->
    case predicate(x) {
      True -> all(rest, predicate)
      _ -> False
    }
  }
}

/// Returns True if the given function returns True for any the elements in
/// the given list. If the function returns True for any of the elements it
/// immediately returns True without checking the rest of the list.
///
/// ## Examples
///
///    > any([], fn(x) { x > 3 })
///    False
///
///    > any([4, 5], fn(x) { x > 3 })
///    False
///
///    > any([4, 3], fn(x) { x > 3 })
///    True
///
///    > any([3, 4], fn(x) { x > 3 })
///    True
///
///
pub fn any(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> False
    [x | rest] ->
    case predicate(x) {
      False -> any(rest, predicate)
      _ -> True
    }
  }
}

/// Takes two lists and returns a single list of 2 item tuples.
///
/// If one of the lists is longer than the other the remaining elements from
/// the longer list are not used.
///
/// ## Examples
///
///    > zip([], [])
///    []
///
///    > zip([1, 2], [3])
///    [tuple(1, 3)]
///
///    > zip([1], [3, 4])
///    [tuple(1, 3)]
///
///    > zip([1, 2], [3, 4])
///    [tuple(1, 3), tuple(2, 4)]
///
///
pub fn zip(xs: List(a), ys: List(b)) -> List(tuple(a, b)) {
  case xs, ys {
    [], _ -> []
    _, [] -> []
    [x | xs], [y | ys] -> [tuple(x, y) | zip(xs, ys)]
  }
}

/// Takes two lists and returns a single list of 2 item tuples.
///
/// If one of the lists is longer than the other an Error is returned.
///
/// ## Examples
///
///    > strict_zip([], [])
///    Ok([])
///
///    > strict_zip([1, 2], [3])
///    Error(LengthMismatch)
///
///    > strict_zip([1], [3, 4])
///    Error(LengthMismatch)
///
///    > strict_zip([1, 2], [3, 4])
///    Ok([tuple(1, 3), tuple(2, 4)])
///
///
pub fn strict_zip(l1: List(a), l2: List(b)) -> Result(List(tuple(a, b)), LengthMismatch) {
  case length(of: l1) == length(of: l2) {
    True -> Ok(zip(l1, l2))
    False -> Error(LengthMismatch)
  }
}

/// Insert a given value between each existing element in a given list.
///
/// This function runs in linear time and copies the list.
///
/// ## Examples
///
///    > intersperse([1, 1, 1], 2)
///    [1, 2, 1, 2, 1]
///
///    > intersperse([], 2)
///    []
///
///
pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [x | rest] -> [x | [elem | intersperse(rest, elem)]]
  }
}

/// Return the element in the Nth position in the list, with 0 being the first
/// position.
///
/// Error(Nil) is returned if the list is not long enough for the given index.
///
/// ## Examples
///
///    > at([1, 2, 3], 1)
///    Ok(2)
///
///    > at([1, 2, 3], 5)
///    Error(Nil)
///
///
pub fn at(in list: List(a), get index: Int) -> Option(a) {
  case index < 0 {
    True -> result.none()
    False ->
    case list {
      [] -> result.none()
      [x | rest] ->
        case index == 0 {
          True -> Ok(x)
          False -> at(rest, index - 1)
        }
    }
  }
}

/// Remove any duplicate elements from a given list.
///
/// This function returns in log-linear time (n log n).
///
/// ## Examples
///
///    > unique([1, 1, 1, 4, 7, 3, 3, 4])
///    [1, 4, 7, 3]
///
///
pub fn unique(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [x | rest] -> [x | unique(filter(rest, fn(y) { y != x }))]
  }
}

fn merge_sort(a: List(a), b: List(a), compare: fn(a, a) -> Order) -> List(a) {
  case a, b {
    [], _ -> b
    _, [] -> a
    [ax | ar], [bx | br] ->
      case compare(ax, bx) {
        order.Lt -> [ax | merge_sort(ar, b, compare)]
        _ -> [bx | merge_sort(a, br, compare)]
      }
  }
}

fn do_sort(list: List(a), compare: fn(a, a) -> Order, list_length: Int) -> List(a) {
  case list_length < 2 {
    True -> list
    False -> {
      let split_length = list_length / 2
      let a_list = take(list, split_length)
      let b_list = drop(list, split_length)
      merge_sort(
        do_sort(a_list, compare, split_length),
        do_sort(b_list, compare, list_length - split_length),
        compare,
      )
    }
  }
}

/// Sort from smallest to largest based upon the ordering specified by a given
/// function.
///
pub fn sort(list: List(a), sort_by compare: fn(a, a) -> Order) -> List(a) {
  do_sort(list, compare, length(list))
}

/// Create a list of ints ranging from a given start and finish.
///
/// ## Examples
///
///    > range(0, 0)
///    []
///
///    > range(0, 5)
///    [0, 1, 2, 3, 4]
///
///    > range(1, -5)
///    [1, 0, -1, -2, -3, -4]
///
///
pub fn range(from start: Int, to stop: Int) -> List(Int) {
  case int.compare(start, stop) {
    order.Eq -> []
    order.Gt -> [start | range(start - 1, stop)]
    order.Lt -> [start | range(start + 1, stop)]
  }
}

fn do_repeat(a: a, times: Int, acc: List(a)) -> List(a) {
  case times <= 0 {
    True -> acc
    False -> do_repeat(a, times - 1, [a | acc])
  }
}

/// Build a list of a given value a given number of times.
///
/// ## Examples
///
///    > repeat("a", times: 0)
///    []
///
///    > repeat("a", times: 5)
///    ["a", "a", "a", "a", "a"]
///
pub fn repeat(item a: a, times times: Int) -> List(a) {
  do_repeat(a, times, [])
}

fn do_split(list: List(a), n: Int, taken: List(a)) -> tuple(List(a), List(a)) {
  case n <= 0 {
    True -> tuple(reverse(taken), list)
    False ->
      case list {
        [] -> tuple(reverse(taken), [])
        [x | xs] -> do_split(xs, n - 1, [x | taken])
      }
  }
}

pub fn split(list list: List(a), at index: Int) -> tuple(List(a), List(a)) {
  do_split(list, index, [])
}

fn do_split_while(
  list: List(a),
  f: fn(a) -> Bool,
  acc: List(a),
) -> tuple(List(a), List(a)) {
  case list {
    [] -> tuple(reverse(acc), [])
    [x | xs] ->
      case f(x) {
        False -> tuple(reverse(acc), list)
        _ -> do_split_while(xs, f, [x | acc])
      }
  }
}

pub fn split_while(
  list list: List(a),
  while predicate: fn(a) -> Bool,
) -> tuple(List(a), List(a)) {
  do_split_while(list, predicate, [])
}

pub fn key_find(
  in keyword_list: List(tuple(k, v)),
  find desired_key: k,
) -> Option(v) {
  find_map(keyword_list, fn(keyword) {
    let tuple(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> result.none()
    }
  })
}

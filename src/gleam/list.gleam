//// Lists are an ordered sequence of elements and are one of the most common
//// data types in Gleam.
////
//// New elements can be added and removed from the front of a list in
//// constant time, while adding and removing from the end requires traversing
//// and copying the whole list, so keep this in mind when designing your
//// programs.
////
//// There is a dedicated syntax for prefixing to a list:
////
//// ```gleam
//// let new_list = [1, 2, ..existing_list]
//// ```
////
//// And a matching syntax for getting the first elements of a list:
////
//// ```gleam
//// case list {
////   [first_element, ..rest] -> first_element
////   _ -> "this pattern matches when the list is empty"
//// }
//// ```
////

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/order.{type Order}

/// Counts the number of elements in a given list.
///
/// This function has to traverse the list to determine the number of elements,
/// so it runs in linear time.
///
/// This function is natively implemented by the virtual machine and is highly
/// optimised.
///
/// ## Examples
///
/// ```gleam
/// length([])
/// // -> 0
/// ```
///
/// ```gleam
/// length([1])
/// // -> 1
/// ```
///
/// ```gleam
/// length([1, 2])
/// // -> 2
/// ```
///
@external(erlang, "erlang", "length")
pub fn length(of list: List(a)) -> Int {
  length_loop(list, 0)
}

fn length_loop(list: List(a), count: Int) -> Int {
  case list {
    [_, ..list] -> length_loop(list, count + 1)
    [] -> count
  }
}

/// Counts the number of elements in a given list satisfying a given predicate.
///
/// This function has to traverse the list to determine the number of elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// count([], fn(a) { a > 0 })
/// // -> 0
/// ```
///
/// ```gleam
/// count([1], fn(a) { a > 0 })
/// // -> 1
/// ```
///
/// ```gleam
/// count([1, 2, 3], int.is_odd)
/// // -> 2
/// ```
///
pub fn count(list: List(a), where predicate: fn(a) -> Bool) -> Int {
  count_loop(list, predicate, 0)
}

fn count_loop(list: List(a), predicate: fn(a) -> Bool, acc: Int) -> Int {
  case list {
    [] -> acc
    [first, ..rest] ->
      case predicate(first) {
        True -> count_loop(rest, predicate, acc + 1)
        False -> count_loop(rest, predicate, acc)
      }
  }
}

/// Creates a new list from a given list containing the same elements but in the
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
/// ```gleam
/// reverse([])
/// // -> []
/// ```
///
/// ```gleam
/// reverse([1])
/// // -> [1]
/// ```
///
/// ```gleam
/// reverse([1, 2])
/// // -> [2, 1]
/// ```
///
@external(erlang, "lists", "reverse")
pub fn reverse(list: List(a)) -> List(a) {
  reverse_and_prepend(list, [])
}

/// Reverses a list and prepends it to another list.
/// This function runs in linear time, proportional to the length of the list
/// to prepend.
///
@external(erlang, "lists", "reverse")
fn reverse_and_prepend(list prefix: List(a), to suffix: List(a)) -> List(a) {
  case prefix {
    [] -> suffix
    [first, ..rest] -> reverse_and_prepend(list: rest, to: [first, ..suffix])
  }
}

/// Determines whether or not the list is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// is_empty([])
/// // -> True
/// ```
///
/// ```gleam
/// is_empty([1])
/// // -> False
/// ```
///
/// ```gleam
/// is_empty([1, 1])
/// // -> False
/// ```
///
pub fn is_empty(list: List(a)) -> Bool {
  list == []
}

/// Determines whether or not a given element exists within a given list.
///
/// This function traverses the list to find the element, so it runs in linear
/// time.
///
/// ## Examples
///
/// ```gleam
/// [] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [0] |> contains(any: 0)
/// // -> True
/// ```
///
/// ```gleam
/// [1] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [1, 1] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [1, 0] |> contains(any: 0)
/// // -> True
/// ```
///
pub fn contains(list: List(a), any elem: a) -> Bool {
  case list {
    [] -> False
    [first, ..] if first == elem -> True
    [_, ..rest] -> contains(rest, elem)
  }
}

/// Gets the first element from the start of the list, if there is one.
///
/// ## Examples
///
/// ```gleam
/// first([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// first([0])
/// // -> Ok(0)
/// ```
///
/// ```gleam
/// first([1, 2])
/// // -> Ok(1)
/// ```
///
pub fn first(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..] -> Ok(first)
  }
}

/// Returns the list minus the first element. If the list is empty, `Error(Nil)` is
/// returned.
///
/// This function runs in constant time and does not make a copy of the list.
///
/// ## Examples
///
/// ```gleam
/// rest([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// rest([0])
/// // -> Ok([])
/// ```
///
/// ```gleam
/// rest([1, 2])
/// // -> Ok([2])
/// ```
///
pub fn rest(list: List(a)) -> Result(List(a), Nil) {
  case list {
    [] -> Error(Nil)
    [_, ..rest] -> Ok(rest)
  }
}

/// Groups the elements from the given list by the given key function.
///
/// Does not preserve the initial value order.
///
/// ## Examples
///
/// ```gleam
/// import gleam/dict
///
/// [Ok(3), Error("Wrong"), Ok(200), Ok(73)]
/// |> group(by: fn(i) {
///   case i {
///     Ok(_) -> "Successful"
///     Error(_) -> "Failed"
///   }
/// })
/// |> dict.to_list
/// // -> [
/// //   #("Failed", [Error("Wrong")]),
/// //   #("Successful", [Ok(73), Ok(200), Ok(3)])
/// // ]
/// ```
///
/// ```gleam
/// import gleam/dict
///
/// group([1,2,3,4,5], by: fn(i) { i - i / 3 * 3 })
/// |> dict.to_list
/// // -> [#(0, [3]), #(1, [4, 1]), #(2, [5, 2])]
/// ```
///
pub fn group(list: List(v), by key: fn(v) -> k) -> Dict(k, List(v)) {
  group_loop(list, key, dict.new())
}

fn group_loop(
  list: List(v),
  to_key: fn(v) -> k,
  groups: Dict(k, List(v)),
) -> Dict(k, List(v)) {
  case list {
    [] -> groups
    [first, ..rest] -> {
      let key = to_key(first)
      let groups = case dict.get(groups, key) {
        Error(_) -> dict.insert(groups, key, [first])
        Ok(existing) -> dict.insert(groups, key, [first, ..existing])
      }
      group_loop(rest, to_key, groups)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `True`.
///
/// ## Examples
///
/// ```gleam
/// filter([2, 4, 6, 1], fn(x) { x > 2 })
/// // -> [4, 6]
/// ```
///
/// ```gleam
/// filter([2, 4, 6, 1], fn(x) { x > 6 })
/// // -> []
/// ```
///
pub fn filter(list: List(a), keeping predicate: fn(a) -> Bool) -> List(a) {
  filter_loop(list, predicate, [])
}

fn filter_loop(list: List(a), fun: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let new_acc = case fun(first) {
        True -> [first, ..acc]
        False -> acc
      }
      filter_loop(rest, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `Ok(_)`.
///
/// ## Examples
///
/// ```gleam
/// filter_map([2, 4, 6, 1], Error)
/// // -> []
/// ```
///
/// ```gleam
/// filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) })
/// // -> [3, 5, 7, 2]
/// ```
///
pub fn filter_map(list: List(a), with fun: fn(a) -> Result(b, e)) -> List(b) {
  filter_map_loop(list, fun, [])
}

fn filter_map_loop(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let new_acc = case fun(first) {
        Ok(first) -> [first, ..acc]
        Error(_) -> acc
      }
      filter_map_loop(rest, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one.
///
/// ## Examples
///
/// ```gleam
/// map([2, 4, 6], fn(x) { x * 2 })
/// // -> [4, 8, 12]
/// ```
///
pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  map_loop(list, fun, [])
}

fn map_loop(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> map_loop(rest, fun, [fun(first), ..acc])
  }
}

/// Combines two lists into a single list using the given function.
///
/// If a list is longer than the other the extra elements are dropped.
///
/// ## Examples
///
/// ```gleam
/// map2([1, 2, 3], [4, 5, 6], fn(x, y) { x + y })
/// // -> [5, 7, 9]
/// ```
///
/// ```gleam
/// map2([1, 2], ["a", "b", "c"], fn(i, x) { #(i, x) })
/// // -> [#(1, "a"), #(2, "b")]
/// ```
///
pub fn map2(list1: List(a), list2: List(b), with fun: fn(a, b) -> c) -> List(c) {
  map2_loop(list1, list2, fun, [])
}

fn map2_loop(
  list1: List(a),
  list2: List(b),
  fun: fn(a, b) -> c,
  acc: List(c),
) -> List(c) {
  case list1, list2 {
    [], _ | _, [] -> reverse(acc)
    [a, ..as_], [b, ..bs] -> map2_loop(as_, bs, fun, [fun(a, b), ..acc])
  }
}

/// Similar to `map` but also lets you pass around an accumulated value.
///
/// ## Examples
///
/// ```gleam
/// map_fold(
///   over: [1, 2, 3],
///   from: 100,
///   with: fn(memo, i) { #(memo + i, i * 2) }
/// )
/// // -> #(106, [2, 4, 6])
/// ```
///
pub fn map_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> #(acc, b),
) -> #(acc, List(b)) {
  map_fold_loop(list, fun, initial, [])
}

fn map_fold_loop(
  list: List(a),
  fun: fn(acc, a) -> #(acc, b),
  acc: acc,
  list_acc: List(b),
) -> #(acc, List(b)) {
  case list {
    [] -> #(acc, reverse(list_acc))
    [first, ..rest] -> {
      let #(acc, first) = fun(acc, first)
      map_fold_loop(rest, fun, acc, [first, ..list_acc])
    }
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
/// ```gleam
/// index_map(["a", "b"], fn(x, i) { #(i, x) })
/// // -> [#(0, "a"), #(1, "b")]
/// ```
///
pub fn index_map(list: List(a), with fun: fn(a, Int) -> b) -> List(b) {
  index_map_loop(list, fun, 0, [])
}

fn index_map_loop(
  list: List(a),
  fun: fn(a, Int) -> b,
  index: Int,
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let acc = [fun(first, index), ..acc]
      index_map_loop(rest, fun, index + 1, acc)
    }
  }
}

/// Takes a function that returns a `Result` and applies it to each element in a
/// given list in turn.
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
/// ```gleam
/// try_map([1, 2, 3], fn(x) { Ok(x + 2) })
/// // -> Ok([3, 4, 5])
/// ```
///
/// ```gleam
/// try_map([1, 2, 3], fn(_) { Error(0) })
/// // -> Error(0)
/// ```
///
/// ```gleam
/// try_map([[1], [2, 3]], first)
/// // -> Ok([1, 2])
/// ```
///
/// ```gleam
/// try_map([[1], [], [2]], first)
/// // -> Error(Nil)
/// ```
///
pub fn try_map(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  try_map_loop(list, fun, [])
}

fn try_map_loop(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(reverse(acc))
    [first, ..rest] ->
      case fun(first) {
        Ok(first) -> try_map_loop(rest, fun, [first, ..acc])
        Error(error) -> Error(error)
      }
  }
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
/// ```gleam
/// drop([1, 2, 3, 4], 2)
/// // -> [3, 4]
/// ```
///
/// ```gleam
/// drop([1, 2, 3, 4], 9)
/// // -> []
/// ```
///
pub fn drop(from list: List(a), up_to n: Int) -> List(a) {
  case n <= 0 {
    True -> list
    False ->
      case list {
        [] -> []
        [_, ..rest] -> drop(rest, n - 1)
      }
  }
}

/// Returns a list containing the first given number of elements from the given
/// list.
///
/// If the element has less than the number of elements then the full list is
/// returned.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// take([1, 2, 3, 4], 2)
/// // -> [1, 2]
/// ```
///
/// ```gleam
/// take([1, 2, 3, 4], 9)
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  take_loop(list, n, [])
}

fn take_loop(list: List(a), n: Int, acc: List(a)) -> List(a) {
  case n <= 0 {
    True -> reverse(acc)
    False ->
      case list {
        [] -> reverse(acc)
        [first, ..rest] -> take_loop(rest, n - 1, [first, ..acc])
      }
  }
}

/// Returns a new empty list.
///
/// ## Examples
///
/// ```gleam
/// new()
/// // -> []
/// ```
///
pub fn new() -> List(a) {
  []
}

/// Returns the given item wrapped in a list.
///
/// ## Examples
///
/// ```gleam
/// wrap(1)
/// // -> [1]
///
/// wrap(["a", "b", "c"])
/// // -> [["a", "b", "c"]]
///
/// wrap([[]])
/// // -> [[[]]]
/// ```
///
///
pub fn wrap(item: a) -> List(a) {
  [item]
}

/// Joins one list onto the end of another.
///
/// This function runs in linear time, and it traverses and copies the first
/// list.
///
/// ## Examples
///
/// ```gleam
/// append([1, 2], [3])
/// // -> [1, 2, 3]
/// ```
///
@external(erlang, "lists", "append")
pub fn append(first: List(a), second: List(a)) -> List(a) {
  append_loop(reverse(first), second)
}

fn append_loop(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [first, ..rest] -> append_loop(rest, [first, ..second])
  }
}

/// Prefixes an item to a list. This can also be done using the dedicated
/// syntax instead
///
/// ```gleam
/// let existing_list = [2, 3, 4]
///
/// [1, ..existing_list]
/// // -> [1, 2, 3, 4]
///
/// prepend(to: existing_list, this: 1)
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn prepend(to list: List(a), this item: a) -> List(a) {
  [item, ..list]
}

/// Joins a list of lists into a single list.
///
/// This function traverses all elements twice on the JavaScript target.
/// This function traverses all elements once on the Erlang target.
///
/// ## Examples
///
/// ```gleam
/// flatten([[1], [2, 3], []])
/// // -> [1, 2, 3]
/// ```
///
@external(erlang, "lists", "append")
pub fn flatten(lists: List(List(a))) -> List(a) {
  flatten_loop(lists, [])
}

fn flatten_loop(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> reverse(acc)
    [list, ..further_lists] ->
      flatten_loop(further_lists, reverse_and_prepend(list, to: acc))
  }
}

/// Maps the list with the given function into a list of lists, and then flattens it.
///
/// ## Examples
///
/// ```gleam
/// flat_map([2, 4, 6], fn(x) { [x, x + 1] })
/// // -> [2, 3, 4, 5, 6, 7]
/// ```
///
pub fn flat_map(over list: List(a), with fun: fn(a) -> List(b)) -> List(b) {
  flatten(map(list, fun))
}

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from left to right.
///
/// `fold([1, 2, 3], 0, add)` is the equivalent of
/// `add(add(add(0, 1), 2), 3)`.
///
/// This function runs in linear time.
///
pub fn fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [first, ..rest] -> fold(rest, fun(initial, first), fun)
  }
}

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from right to left.
///
/// `fold_right([1, 2, 3], 0, add)` is the equivalent of
/// `add(add(add(0, 3), 2), 1)`.
///
/// This function runs in linear time.
///
/// Unlike `fold` this function is not tail recursive. Where possible use
/// `fold` instead as it will use less memory.
///
pub fn fold_right(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [first, ..rest] -> fun(fold_right(rest, initial, fun), first)
  }
}

/// Like fold but the folding function also receives the index of the current element.
///
/// ## Examples
///
/// ```gleam
/// ["a", "b", "c"]
/// |> index_fold([], fn(acc, item, index) { ... })
/// ```
///
pub fn index_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a, Int) -> acc,
) -> acc {
  index_fold_loop(list, initial, fun, 0)
}

fn index_fold_loop(
  over: List(a),
  acc: acc,
  with: fn(acc, a, Int) -> acc,
  index: Int,
) -> acc {
  case over {
    [] -> acc
    [first, ..rest] ->
      index_fold_loop(rest, with(acc, first, index), with, index + 1)
  }
}

/// A variant of fold that might fail.
///
/// The folding function should return `Result(accumulator, error)`.
/// If the returned value is `Ok(accumulator)` try_fold will try the next value in the list.
/// If the returned value is `Error(error)` try_fold will stop and return that error.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4]
/// |> try_fold(0, fn(acc, i) {
///   case i < 3 {
///     True -> Ok(acc + i)
///     False -> Error(Nil)
///   }
/// })
/// // -> Error(Nil)
/// ```
///
pub fn try_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> Result(acc, e),
) -> Result(acc, e) {
  case list {
    [] -> Ok(initial)
    [first, ..rest] ->
      case fun(initial, first) {
        Ok(result) -> try_fold(rest, result, fun)
        Error(_) as error -> error
      }
  }
}

pub type ContinueOrStop(a) {
  Continue(a)
  Stop(a)
}

/// A variant of fold that allows to stop folding earlier.
///
/// The folding function should return `ContinueOrStop(accumulator)`.
/// If the returned value is `Continue(accumulator)` fold_until will try the next value in the list.
/// If the returned value is `Stop(accumulator)` fold_until will stop and return that accumulator.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4]
/// |> fold_until(0, fn(acc, i) {
///   case i < 3 {
///     True -> Continue(acc + i)
///     False -> Stop(acc)
///   }
/// })
/// // -> 3
/// ```
///
pub fn fold_until(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> ContinueOrStop(acc),
) -> acc {
  case list {
    [] -> initial
    [first, ..rest] ->
      case fun(initial, first) {
        Continue(next_accumulator) -> fold_until(rest, next_accumulator, fun)
        Stop(b) -> b
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// `True`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find([1, 2, 3], fn(x) { x > 2 })
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// find([1, 2, 3], fn(x) { x > 4 })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find([], fn(_) { True })
/// // -> Error(Nil)
/// ```
///
pub fn find(
  in list: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case is_desired(first) {
        True -> Ok(first)
        False -> find(in: rest, one_that: is_desired)
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// `Ok(new_value)`, then returns the wrapped `new_value`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find_map([[], [2], [3]], first)
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// find_map([[], []], first)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find_map([], first)
/// // -> Error(Nil)
/// ```
///
pub fn find_map(
  in list: List(a),
  with fun: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case fun(first) {
        Ok(first) -> Ok(first)
        Error(_) -> find_map(in: rest, with: fun)
      }
  }
}

/// Returns `True` if the given function returns `True` for all the elements in
/// the given list. If the function returns `False` for any of the elements it
/// immediately returns `False` without checking the rest of the list.
///
/// ## Examples
///
/// ```gleam
/// all([], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// all([4, 5], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// all([4, 3], fn(x) { x > 3 })
/// // -> False
/// ```
///
pub fn all(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [first, ..rest] ->
      case predicate(first) {
        True -> all(rest, predicate)
        False -> False
      }
  }
}

/// Returns `True` if the given function returns `True` for any the elements in
/// the given list. If the function returns `True` for any of the elements it
/// immediately returns `True` without checking the rest of the list.
///
/// ## Examples
///
/// ```gleam
/// any([], fn(x) { x > 3 })
/// // -> False
/// ```
///
/// ```gleam
/// any([4, 5], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// any([4, 3], fn(x) { x > 4 })
/// // -> False
/// ```
///
/// ```gleam
/// any([3, 4], fn(x) { x > 3 })
/// // -> True
/// ```
///
pub fn any(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> False
    [first, ..rest] ->
      case predicate(first) {
        True -> True
        False -> any(rest, predicate)
      }
  }
}

/// Takes two lists and returns a single list of 2-element tuples.
///
/// If one of the lists is longer than the other, the remaining elements from
/// the longer list are not used.
///
/// ## Examples
///
/// ```gleam
/// zip([], [])
/// // -> []
/// ```
///
/// ```gleam
/// zip([1, 2], [3])
/// // -> [#(1, 3)]
/// ```
///
/// ```gleam
/// zip([1], [3, 4])
/// // -> [#(1, 3)]
/// ```
///
/// ```gleam
/// zip([1, 2], [3, 4])
/// // -> [#(1, 3), #(2, 4)]
/// ```
///
pub fn zip(list: List(a), with other: List(b)) -> List(#(a, b)) {
  zip_loop(list, other, [])
}

fn zip_loop(one: List(a), other: List(b), acc: List(#(a, b))) -> List(#(a, b)) {
  case one, other {
    [first_one, ..rest_one], [first_other, ..rest_other] ->
      zip_loop(rest_one, rest_other, [#(first_one, first_other), ..acc])
    _, _ -> reverse(acc)
  }
}

/// Takes two lists and returns a single list of 2-element tuples.
///
/// If one of the lists is longer than the other, an `Error` is returned.
///
/// ## Examples
///
/// ```gleam
/// strict_zip([], [])
/// // -> Ok([])
/// ```
///
/// ```gleam
/// strict_zip([1, 2], [3])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// strict_zip([1], [3, 4])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// strict_zip([1, 2], [3, 4])
/// // -> Ok([#(1, 3), #(2, 4)])
/// ```
///
pub fn strict_zip(
  list: List(a),
  with other: List(b),
) -> Result(List(#(a, b)), Nil) {
  strict_zip_loop(list, other, [])
}

fn strict_zip_loop(
  one: List(a),
  other: List(b),
  acc: List(#(a, b)),
) -> Result(List(#(a, b)), Nil) {
  case one, other {
    [], [] -> Ok(reverse(acc))
    [], _ | _, [] -> Error(Nil)
    [first_one, ..rest_one], [first_other, ..rest_other] ->
      strict_zip_loop(rest_one, rest_other, [#(first_one, first_other), ..acc])
  }
}

/// Takes a single list of 2-element tuples and returns two lists.
///
/// ## Examples
///
/// ```gleam
/// unzip([#(1, 2), #(3, 4)])
/// // -> #([1, 3], [2, 4])
/// ```
///
/// ```gleam
/// unzip([])
/// // -> #([], [])
/// ```
///
pub fn unzip(input: List(#(a, b))) -> #(List(a), List(b)) {
  unzip_loop(input, [], [])
}

fn unzip_loop(
  input: List(#(a, b)),
  one: List(a),
  other: List(b),
) -> #(List(a), List(b)) {
  case input {
    [] -> #(reverse(one), reverse(other))
    [#(first_one, first_other), ..rest] ->
      unzip_loop(rest, [first_one, ..one], [first_other, ..other])
  }
}

/// Inserts a given value between each existing element in a given list.
///
/// This function runs in linear time and copies the list.
///
/// ## Examples
///
/// ```gleam
/// intersperse([1, 1, 1], 2)
/// // -> [1, 2, 1, 2, 1]
/// ```
///
/// ```gleam
/// intersperse([], 2)
/// // -> []
/// ```
///
pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [first, ..rest] -> intersperse_loop(rest, elem, [first])
  }
}

fn intersperse_loop(list: List(a), separator: a, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] ->
      intersperse_loop(rest, separator, [first, separator, ..acc])
  }
}

/// Removes any duplicate elements from a given list.
///
/// This function returns in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// unique([1, 1, 1, 4, 7, 3, 3, 4])
/// // -> [1, 4, 7, 3]
/// ```
///
pub fn unique(list: List(a)) -> List(a) {
  unique_loop(list, dict.new(), [])
}

fn unique_loop(list: List(a), seen: Dict(a, Nil), acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] ->
      case dict.has_key(seen, first) {
        True -> unique_loop(rest, seen, acc)
        False ->
          unique_loop(rest, dict.insert(seen, first, Nil), [first, ..acc])
      }
  }
}

/// Sorts from smallest to largest based upon the ordering specified by a given
/// function.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// sort([4, 3, 6, 5, 4, 1, 2], by: int.compare)
/// // -> [1, 2, 3, 4, 4, 5, 6]
/// ```
///
pub fn sort(list: List(a), by compare: fn(a, a) -> Order) -> List(a) {
  // This is a natural, tail recursive, stable merge sort:
  // - natural: it is very efficient if you call it on a list that is already
  //   (pre)sorted because it works on slices of the original list.
  // - tail recursive: the stack won't grow linearly with the size of the list.
  // - stable: if two items are considered to be equal then their original
  //   relative order is preserved.
  case list {
    // If the list has zero/one item then it's already sorted.
    [] -> []
    [x] -> [x]

    // Otherwise the algorithm works as follow: we split the list in sequences
    // of already sorted values as they appear in the list and then we merge
    // those together two by two using `merge_all`.
    [x, y, ..rest] -> {
      // We need to compare the first two items to properly call `sequences`
      // with the correct initial values. If the second item is <= than the
      // first, then we know we'll start by growing a descending sequence
      // (and an ascending one in the opposite case).
      let direction = case compare(x, y) {
        order.Lt | order.Eq -> Ascending
        order.Gt -> Descending
      }

      // `sequences` produces sequences in ascending order so we call the
      // `merge_all` function saying it to expect all sequences to be sorted
      // that way.
      let sequences = sequences(rest, compare, [x], direction, y, [])
      merge_all(sequences, Ascending, compare)
    }
  }
}

type Sorting {
  Ascending
  Descending
}

/// Given a list it returns slices of it that are locally sorted in ascending
/// order.
///
/// Imagine you have this list:
///
/// ```
///   [1, 2, 3, 2, 1, 0]
///    ^^^^^^^  ^^^^^^^ This is a slice in descending order
///    |
///    | This is a slice that is sorted in ascending order
/// ```
///
/// So the produced result will contain these two slices, each one sorted in
/// ascending order: `[[1, 2, 3], [0, 1, 2]]`.
///
/// - `growing` is an accumulator with the current slice being grown
/// - `direction` is the growing direction of the slice being grown, it could
///   either be ascending or strictly descending
/// - `prev` is the previous element that needs to be added to the growing slice
///   it is carried around to check whether we have to keep growing the current
///   slice or not
/// - `acc` is the accumulator containing the slices sorted in ascending order
///
fn sequences(
  list: List(a),
  compare: fn(a, a) -> Order,
  growing: List(a),
  direction: Sorting,
  prev: a,
  acc: List(List(a)),
) -> List(List(a)) {
  // First of all we must not forget to add the previous element to the
  // currently growing slice.
  let growing = [prev, ..growing]

  case list {
    [] ->
      case direction {
        // Notice how we have to reverse the accumulator we're growing: since
        // we always add items to the head, `growing` is built in the opposite
        // sorting order of what it actually is in the original list.
        Ascending -> [reverse(growing), ..acc]
        Descending -> [growing, ..acc]
      }

    [new, ..rest] ->
      case compare(prev, new), direction {
        // In case the new element respects the ordering of the growing
        // sequence, then we just keep growing it.
        // Notice how a growing sequence is weakly growing (that is it can have
        // consecutive equal items) while a decreasing sequence is strictly
        // decreasing (no consecutive equal items), this is needed to make the
        // algorithm stable!
        order.Gt, Descending | order.Lt, Ascending | order.Eq, Ascending ->
          sequences(rest, compare, growing, direction, new, acc)

        // We were growing an ascending (descending) sequence and the new item
        // is smaller (bigger) than the previous one, this means we have to stop
        // growing this sequence and start with a new one whose first item will
        // be the one we just found.
        order.Gt, Ascending | order.Lt, Descending | order.Eq, Descending -> {
          let acc = case direction {
            Ascending -> [reverse(growing), ..acc]
            Descending -> [growing, ..acc]
          }
          case rest {
            // The list is over so we just create a sequence containing the last
            // item we saw and add it to the accumulator before returning it.
            [] -> [[new], ..acc]

            // If the list is not over we have a peek at the next item to decide
            // in which direction is growing the new sequence and make the
            // recursive call with the appropriate arguments.
            [next, ..rest] -> {
              let direction = case compare(new, next) {
                order.Lt | order.Eq -> Ascending
                order.Gt -> Descending
              }
              sequences(rest, compare, [new], direction, next, acc)
            }
          }
        }
      }
  }
}

/// Given some some sorted sequences (assumed to be sorted in `direction`) it
/// merges them all together until we're left with just a list sorted in
/// ascending order.
///
fn merge_all(
  sequences: List(List(a)),
  direction: Sorting,
  compare: fn(a, a) -> Order,
) -> List(a) {
  case sequences, direction {
    [], _ -> []

    // If we have a single list in ascending order then we're done.
    [sequence], Ascending -> sequence

    // If we have a single list in descending order, we reverse it to make sure
    // it's in ascending order and we're done.
    [sequence], Descending -> reverse(sequence)

    // Merging together sequences that are in ascending (descending) order
    // reverses their order, so the recursive call will assume to be merging
    // lists sorted in the opposite order!
    _, Ascending -> {
      let sequences = merge_ascending_pairs(sequences, compare, [])
      merge_all(sequences, Descending, compare)
    }

    _, Descending -> {
      let sequences = merge_descending_pairs(sequences, compare, [])
      merge_all(sequences, Ascending, compare)
    }
  }
}

/// Given a list of ascending lists, it merges adjacent pairs into a single
/// descending list, halving their number.
/// It returns a list of the remaining descending lists.
///
fn merge_ascending_pairs(
  sequences: List(List(a)),
  compare: fn(a, a) -> Order,
  acc: List(List(a)),
) {
  case sequences {
    [] -> reverse(acc)

    // Beware, if we have just one item left we must reverse it: we take
    // ascending lists as input and have to return descending ones.
    // If we returned it like it is it would be sorted in ascending order.
    [sequence] -> reverse([reverse(sequence), ..acc])

    [ascending1, ascending2, ..rest] -> {
      let descending = merge_ascendings(ascending1, ascending2, compare, [])
      merge_ascending_pairs(rest, compare, [descending, ..acc])
    }
  }
}

/// This is the same as merge_ascending_pairs but flipped for descending lists.
///
fn merge_descending_pairs(
  sequences: List(List(a)),
  compare: fn(a, a) -> Order,
  acc: List(List(a)),
) {
  case sequences {
    [] -> reverse(acc)

    [sequence] -> reverse([reverse(sequence), ..acc])

    [descending1, descending2, ..rest] -> {
      let ascending = merge_descendings(descending1, descending2, compare, [])
      merge_descending_pairs(rest, compare, [ascending, ..acc])
    }
  }
}

/// Merges two lists sorted in ascending order into a single list sorted in
/// descending order according to the given comparator function.
///
/// This reversing of the sort order is not avoidable if we want to implement
/// merge as a tail recursive function. We could reverse the accumulator before
/// returning it but that would end up being less efficient; so the merging
/// algorithm has to play around this.
///
fn merge_ascendings(
  list1: List(a),
  list2: List(a),
  compare: fn(a, a) -> Order,
  acc: List(a),
) -> List(a) {
  case list1, list2 {
    [], list | list, [] -> reverse_and_prepend(list, acc)

    [first1, ..rest1], [first2, ..rest2] ->
      case compare(first1, first2) {
        order.Lt -> merge_ascendings(rest1, list2, compare, [first1, ..acc])
        order.Gt | order.Eq ->
          merge_ascendings(list1, rest2, compare, [first2, ..acc])
      }
  }
}

/// This is exactly the same as merge_ascendings but mirrored: it merges two
/// lists sorted in descending order into a single list sorted in ascending
/// order according to the given comparator function.
///
/// This reversing of the sort order is not avoidable if we want to implement
/// merge as a tail recursive function. We could reverse the accumulator before
/// returning it but that would end up being less efficient; so the merging
/// algorithm has to play around this.
///
fn merge_descendings(
  list1: List(a),
  list2: List(a),
  compare: fn(a, a) -> Order,
  acc: List(a),
) -> List(a) {
  case list1, list2 {
    [], list | list, [] -> reverse_and_prepend(list, acc)
    [first1, ..rest1], [first2, ..rest2] ->
      case compare(first1, first2) {
        order.Lt -> merge_descendings(list1, rest2, compare, [first2, ..acc])
        order.Gt | order.Eq ->
          merge_descendings(rest1, list2, compare, [first1, ..acc])
      }
  }
}

/// Creates a list of ints ranging from a given start and finish.
///
/// ## Examples
///
/// ```gleam
/// range(0, 0)
/// // -> [0]
/// ```
///
/// ```gleam
/// range(0, 5)
/// // -> [0, 1, 2, 3, 4, 5]
/// ```
///
/// ```gleam
/// range(1, -5)
/// // -> [1, 0, -1, -2, -3, -4, -5]
/// ```
///
pub fn range(from start: Int, to stop: Int) -> List(Int) {
  range_loop(start, stop, [])
}

fn range_loop(start: Int, stop: Int, acc: List(Int)) -> List(Int) {
  case int.compare(start, stop) {
    order.Eq -> [stop, ..acc]
    order.Gt -> range_loop(start, stop + 1, [stop, ..acc])
    order.Lt -> range_loop(start, stop - 1, [stop, ..acc])
  }
}

/// Builds a list of a given value a given number of times.
///
/// ## Examples
///
/// ```gleam
/// repeat("a", times: 0)
/// // -> []
/// ```
///
/// ```gleam
/// repeat("a", times: 5)
/// // -> ["a", "a", "a", "a", "a"]
/// ```
///
pub fn repeat(item a: a, times times: Int) -> List(a) {
  repeat_loop(a, times, [])
}

fn repeat_loop(item: a, times: Int, acc: List(a)) -> List(a) {
  case times <= 0 {
    True -> acc
    False -> repeat_loop(item, times - 1, [item, ..acc])
  }
}

/// Splits a list in two before the given index.
///
/// If the list is not long enough to have the given index the before list will
/// be the input list, and the after list will be empty.
///
/// ## Examples
///
/// ```gleam
/// split([6, 7, 8, 9], 0)
/// // -> #([], [6, 7, 8, 9])
/// ```
///
/// ```gleam
/// split([6, 7, 8, 9], 2)
/// // -> #([6, 7], [8, 9])
/// ```
///
/// ```gleam
/// split([6, 7, 8, 9], 4)
/// // -> #([6, 7, 8, 9], [])
/// ```
///
pub fn split(list list: List(a), at index: Int) -> #(List(a), List(a)) {
  split_loop(list, index, [])
}

fn split_loop(list: List(a), n: Int, taken: List(a)) -> #(List(a), List(a)) {
  case n <= 0 {
    True -> #(reverse(taken), list)
    False ->
      case list {
        [] -> #(reverse(taken), [])
        [first, ..rest] -> split_loop(rest, n - 1, [first, ..taken])
      }
  }
}

/// Splits a list in two before the first element that a given function returns
/// `False` for.
///
/// If the function returns `True` for all elements the first list will be the
/// input list, and the second list will be empty.
///
/// ## Examples
///
/// ```gleam
/// split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
/// // -> #([1, 2, 3], [4, 5])
/// ```
///
/// ```gleam
/// split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
/// // -> #([1, 2, 3, 4, 5], [])
/// ```
///
pub fn split_while(
  list list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  split_while_loop(list, predicate, [])
}

fn split_while_loop(
  list: List(a),
  f: fn(a) -> Bool,
  acc: List(a),
) -> #(List(a), List(a)) {
  case list {
    [] -> #(reverse(acc), [])
    [first, ..rest] ->
      case f(first) {
        True -> split_while_loop(rest, f, [first, ..acc])
        False -> #(reverse(acc), list)
      }
  }
}

/// Given a list of 2-element tuples, finds the first tuple that has a given
/// key as the first element and returns the second element.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// This function may be useful for interacting with Erlang code where lists of
/// tuples are common.
///
/// ## Examples
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "a")
/// // -> Ok(0)
/// ```
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "b")
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "c")
/// // -> Error(Nil)
/// ```
///
pub fn key_find(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> Result(v, Nil) {
  find_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}

/// Given a list of 2-element tuples, finds all tuples that have a given
/// key as the first element and returns the second element.
///
/// This function may be useful for interacting with Erlang code where lists of
/// tuples are common.
///
/// ## Examples
///
/// ```gleam
/// key_filter([#("a", 0), #("b", 1), #("a", 2)], "a")
/// // -> [0, 2]
/// ```
///
/// ```gleam
/// key_filter([#("a", 0), #("b", 1)], "c")
/// // -> []
/// ```
///
pub fn key_filter(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> List(v) {
  filter_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}

/// Given a list of 2-element tuples, finds the first tuple that has a given
/// key as the first element. This function will return the second element
/// of the found tuple and list with tuple removed.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// ## Examples
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "a")
/// // -> Ok(#(0, [#("b", 1)]))
/// ```
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "b")
/// // -> Ok(#(1, [#("a", 0)]))
/// ```
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "c")
/// // -> Error(Nil)
/// ```
///
pub fn key_pop(list: List(#(k, v)), key: k) -> Result(#(v, List(#(k, v))), Nil) {
  key_pop_loop(list, key, [])
}

fn key_pop_loop(
  list: List(#(k, v)),
  key: k,
  checked: List(#(k, v)),
) -> Result(#(v, List(#(k, v))), Nil) {
  case list {
    [] -> Error(Nil)
    [#(k, v), ..rest] if k == key ->
      Ok(#(v, reverse_and_prepend(checked, rest)))
    [first, ..rest] -> key_pop_loop(rest, key, [first, ..checked])
  }
}

/// Given a list of 2-element tuples, inserts a key and value into the list.
///
/// If there was already a tuple with the key then it is replaced, otherwise it
/// is added to the end of the list.
///
/// ## Examples
///
/// ```gleam
/// key_set([#(5, 0), #(4, 1)], 4, 100)
/// // -> [#(5, 0), #(4, 100)]
/// ```
///
/// ```gleam
/// key_set([#(5, 0), #(4, 1)], 1, 100)
/// // -> [#(5, 0), #(4, 1), #(1, 100)]
/// ```
///
pub fn key_set(list: List(#(k, v)), key: k, value: v) -> List(#(k, v)) {
  key_set_loop(list, key, value, [])
}

fn key_set_loop(
  list: List(#(k, v)),
  key: k,
  value: v,
  inspected: List(#(k, v)),
) -> List(#(k, v)) {
  case list {
    [#(k, _), ..rest] if k == key ->
      reverse_and_prepend(inspected, [#(k, value), ..rest])
    [first, ..rest] -> key_set_loop(rest, key, value, [first, ..inspected])
    [] -> reverse([#(key, value), ..inspected])
  }
}

/// Calls a function for each element in a list, discarding the return value.
///
/// Useful for calling a side effect for every item of a list.
///
/// ```gleam
/// import gleam/io
///
/// each(["1", "2", "3"], io.println)
/// // -> Nil
/// // 1
/// // 2
/// // 3
/// ```
///
pub fn each(list: List(a), f: fn(a) -> b) -> Nil {
  case list {
    [] -> Nil
    [first, ..rest] -> {
      f(first)
      each(rest, f)
    }
  }
}

/// Calls a `Result` returning function for each element in a list, discarding
/// the return value. If the function returns `Error` then the iteration is
/// stopped and the error is returned.
///
/// Useful for calling a side effect for every item of a list.
///
/// ## Examples
///
/// ```gleam
/// try_each(
///   over: [1, 2, 3],
///   with: function_that_might_fail,
/// )
/// // -> Ok(Nil)
/// ```
///
pub fn try_each(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(Nil, e) {
  case list {
    [] -> Ok(Nil)
    [first, ..rest] ->
      case fun(first) {
        Ok(_) -> try_each(over: rest, with: fun)
        Error(e) -> Error(e)
      }
  }
}

/// Partitions a list into a tuple/pair of lists
/// by a given categorisation function.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// [1, 2, 3, 4, 5] |> partition(int.is_odd)
/// // -> #([1, 3, 5], [2, 4])
/// ```
///
pub fn partition(
  list: List(a),
  with categorise: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  partition_loop(list, categorise, [], [])
}

fn partition_loop(list, categorise, trues, falses) {
  case list {
    [] -> #(reverse(trues), reverse(falses))
    [first, ..rest] ->
      case categorise(first) {
        True -> partition_loop(rest, categorise, [first, ..trues], falses)
        False -> partition_loop(rest, categorise, trues, [first, ..falses])
      }
  }
}

/// Returns all the permutations of a list.
///
/// ## Examples
///
/// ```gleam
/// permutations([1, 2])
/// // -> [[1, 2], [2, 1]]
/// ```
///
pub fn permutations(list: List(a)) -> List(List(a)) {
  case list {
    [] -> [[]]
    l -> permutation_zip(l, [], [])
  }
}

fn permutation_zip(
  list: List(a),
  rest: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [] -> reverse(acc)
    [head, ..tail] ->
      permutation_prepend(
        head,
        permutations(reverse_and_prepend(rest, tail)),
        tail,
        [head, ..rest],
        acc,
      )
  }
}

fn permutation_prepend(
  el: a,
  permutations: List(List(a)),
  list_1: List(a),
  list_2: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case permutations {
    [] -> permutation_zip(list_1, list_2, acc)
    [head, ..tail] ->
      permutation_prepend(el, tail, list_1, list_2, [[el, ..head], ..acc])
  }
}

/// Returns a list of sliding windows.
///
/// ## Examples
///
/// ```gleam
/// window([1,2,3,4,5], 3)
/// // -> [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
/// ```
///
/// ```gleam
/// window([1, 2], 4)
/// // -> []
/// ```
///
pub fn window(list: List(a), by n: Int) -> List(List(a)) {
  case n <= 0 {
    True -> []
    False -> window_loop([], list, n)
  }
}

fn window_loop(acc: List(List(a)), list: List(a), n: Int) -> List(List(a)) {
  let window = take(list, n)

  case length(window) == n {
    True -> window_loop([window, ..acc], drop(list, 1), n)
    False -> reverse(acc)
  }
}

/// Returns a list of tuples containing two contiguous elements.
///
/// ## Examples
///
/// ```gleam
/// window_by_2([1,2,3,4])
/// // -> [#(1, 2), #(2, 3), #(3, 4)]
/// ```
///
/// ```gleam
/// window_by_2([1])
/// // -> []
/// ```
///
pub fn window_by_2(list: List(a)) -> List(#(a, a)) {
  zip(list, drop(list, 1))
}

/// Drops the first elements in a given list for which the predicate function returns `True`.
///
/// ## Examples
///
/// ```gleam
/// drop_while([1, 2, 3, 4], fn (x) { x < 3 })
/// // -> [3, 4]
/// ```
///
pub fn drop_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  case list {
    [] -> []
    [first, ..rest] ->
      case predicate(first) {
        True -> drop_while(rest, predicate)
        False -> [first, ..rest]
      }
  }
}

/// Takes the first elements in a given list for which the predicate function returns `True`.
///
/// ## Examples
///
/// ```gleam
/// take_while([1, 2, 3, 2, 4], fn (x) { x < 3 })
/// // -> [1, 2]
/// ```
///
pub fn take_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  take_while_loop(list, predicate, [])
}

fn take_while_loop(
  list: List(a),
  predicate: fn(a) -> Bool,
  acc: List(a),
) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] ->
      case predicate(first) {
        True -> take_while_loop(rest, predicate, [first, ..acc])
        False -> reverse(acc)
      }
  }
}

/// Returns a list of chunks in which
/// the return value of calling `f` on each element is the same.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 2, 3, 4, 4, 6, 7, 7] |> chunk(by: fn(n) { n % 2 })
/// // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
/// ```
///
pub fn chunk(in list: List(a), by f: fn(a) -> k) -> List(List(a)) {
  case list {
    [] -> []
    [first, ..rest] -> chunk_loop(rest, f, f(first), [first], [])
  }
}

fn chunk_loop(
  list: List(a),
  f: fn(a) -> k,
  previous_key: k,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [first, ..rest] -> {
      let key = f(first)
      case key == previous_key {
        True -> chunk_loop(rest, f, key, [first, ..current_chunk], acc)
        False -> {
          let new_acc = [reverse(current_chunk), ..acc]
          chunk_loop(rest, f, key, [first], new_acc)
        }
      }
    }
    [] -> reverse([reverse(current_chunk), ..acc])
  }
}

/// Returns a list of chunks containing `count` elements each.
///
/// If the last chunk does not have `count` elements, it is instead
/// a partial chunk, with less than `count` elements.
///
/// For any `count` less than 1 this function behaves as if it was set to 1.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4, 5, 6] |> sized_chunk(into: 2)
/// // -> [[1, 2], [3, 4], [5, 6]]
/// ```
///
/// ```gleam
/// [1, 2, 3, 4, 5, 6, 7, 8] |> sized_chunk(into: 3)
/// // -> [[1, 2, 3], [4, 5, 6], [7, 8]]
/// ```
///
pub fn sized_chunk(in list: List(a), into count: Int) -> List(List(a)) {
  sized_chunk_loop(list, count, count, [], [])
}

fn sized_chunk_loop(
  list: List(a),
  count: Int,
  left: Int,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [] ->
      case current_chunk {
        [] -> reverse(acc)
        remaining -> reverse([reverse(remaining), ..acc])
      }
    [first, ..rest] -> {
      let chunk = [first, ..current_chunk]
      case left > 1 {
        True -> sized_chunk_loop(rest, count, left - 1, chunk, acc)
        False ->
          sized_chunk_loop(rest, count, count, [], [reverse(chunk), ..acc])
      }
    }
  }
}

/// This function acts similar to fold, but does not take an initial state.
/// Instead, it starts from the first element in the list
/// and combines it with each subsequent element in turn using the given
/// function. The function is called as `fun(accumulator, current_element)`.
///
/// Returns `Ok` to indicate a successful run, and `Error` if called on an
/// empty list.
///
/// ## Examples
///
/// ```gleam
/// [] |> reduce(fn(acc, x) { acc + x })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// [1, 2, 3, 4, 5] |> reduce(fn(acc, x) { acc + x })
/// // -> Ok(15)
/// ```
///
pub fn reduce(over list: List(a), with fun: fn(a, a) -> a) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] -> Ok(fold(rest, first, fun))
  }
}

/// Similar to `fold`, but yields the state of the accumulator at each stage.
///
/// ## Examples
///
/// ```gleam
/// scan(over: [1, 2, 3], from: 100, with: fn(acc, i) { acc + i })
/// // -> [101, 103, 106]
/// ```
///
pub fn scan(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> List(acc) {
  scan_loop(list, initial, [], fun)
}

fn scan_loop(
  list: List(a),
  accumulator: acc,
  accumulated: List(acc),
  fun: fn(acc, a) -> acc,
) -> List(acc) {
  case list {
    [] -> reverse(accumulated)
    [first, ..rest] -> {
      let next = fun(accumulator, first)
      scan_loop(rest, next, [next, ..accumulated], fun)
    }
  }
}

/// Returns the last element in the given list.
///
/// Returns `Error(Nil)` if the list is empty.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// last([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// last([1, 2, 3, 4, 5])
/// // -> Ok(5)
/// ```
///
pub fn last(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [last] -> Ok(last)
    [_, ..rest] -> last(rest)
  }
}

/// Return unique combinations of elements in the list.
///
/// ## Examples
///
/// ```gleam
/// combinations([1, 2, 3], 2)
/// // -> [[1, 2], [1, 3], [2, 3]]
/// ```
///
/// ```gleam
/// combinations([1, 2, 3, 4], 3)
/// // -> [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
/// ```
///
pub fn combinations(items: List(a), by n: Int) -> List(List(a)) {
  case n, items {
    0, _ -> [[]]
    _, [] -> []
    _, [first, ..rest] ->
      combinations(rest, n - 1)
      |> map(fn(combination) { [first, ..combination] })
      |> reverse
      |> fold(combinations(rest, n), fn(acc, c) { [c, ..acc] })
  }
}

/// Return unique pair combinations of elements in the list.
///
/// ## Examples
///
/// ```gleam
/// combination_pairs([1, 2, 3])
/// // -> [#(1, 2), #(1, 3), #(2, 3)]
/// ```
///
pub fn combination_pairs(items: List(a)) -> List(#(a, a)) {
  combination_pairs_loop(items, [])
}

fn combination_pairs_loop(items: List(a), acc: List(#(a, a))) -> List(#(a, a)) {
  case items {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let first_combinations = map(rest, with: fn(other) { #(first, other) })
      let acc = reverse_and_prepend(first_combinations, acc)
      combination_pairs_loop(rest, acc)
    }
  }
}

/// Make a list alternating the elements from the given lists
///
/// ## Examples
///
/// ```gleam
/// interleave([[1, 2], [101, 102], [201, 202]])
/// // -> [1, 101, 201, 2, 102, 202]
/// ```
///
pub fn interleave(list: List(List(a))) -> List(a) {
  transpose(list)
  |> flatten
}

/// Transpose rows and columns of the list of lists.
///
/// Notice: This function is not tail recursive,
/// and thus may exceed stack size if called,
/// with large lists (on the JavaScript target).
///
/// ## Examples
///
/// ```gleam
/// transpose([[1, 2, 3], [101, 102, 103]])
/// // -> [[1, 101], [2, 102], [3, 103]]
/// ```
///
pub fn transpose(list_of_lists: List(List(a))) -> List(List(a)) {
  transpose_loop(list_of_lists, [])
}

fn transpose_loop(rows: List(List(a)), columns: List(List(a))) -> List(List(a)) {
  case rows {
    [] -> reverse(columns)
    _ -> {
      let #(column, rest) = take_firsts(rows, [], [])
      case column {
        [_, ..] -> transpose_loop(rest, [column, ..columns])
        [] -> transpose_loop(rest, columns)
      }
    }
  }
}

fn take_firsts(
  rows: List(List(a)),
  column: List(a),
  remaining_rows: List(List(a)),
) -> #(List(a), List(List(a))) {
  case rows {
    [] -> #(reverse(column), reverse(remaining_rows))
    [[], ..rest] -> take_firsts(rest, column, remaining_rows)
    [[first, ..remaining_row], ..rest_rows] -> {
      let remaining_rows = [remaining_row, ..remaining_rows]
      take_firsts(rest_rows, [first, ..column], remaining_rows)
    }
  }
}

/// Takes a list, randomly sorts all items and returns the shuffled list.
///
/// This function uses `float.random` to decide the order of the elements.
///
/// ## Example
///
/// ```gleam
/// range(1, 10) |> shuffle()
/// // -> [1, 6, 9, 10, 3, 8, 4, 2, 7, 5]
/// ```
///
pub fn shuffle(list: List(a)) -> List(a) {
  list
  |> fold(from: [], with: fn(acc, a) { [#(float.random(), a), ..acc] })
  |> do_shuffle_by_pair_indexes()
  |> shuffle_pair_unwrap_loop([])
}

fn shuffle_pair_unwrap_loop(list: List(#(Float, a)), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [elem_pair, ..enumerable] ->
      shuffle_pair_unwrap_loop(enumerable, [elem_pair.1, ..acc])
  }
}

fn do_shuffle_by_pair_indexes(
  list_of_pairs: List(#(Float, a)),
) -> List(#(Float, a)) {
  sort(list_of_pairs, fn(a_pair: #(Float, a), b_pair: #(Float, a)) -> Order {
    float.compare(a_pair.0, b_pair.0)
  })
}

/// Takes a list and a comparator, and returns the maximum element in the list
///
///
/// ## Example
///
/// ```gleam
/// range(1, 10) |> list.max(int.compare)
/// // -> Ok(10)
/// ```
///
/// ```gleam
/// ["a", "c", "b"] |> list.max(string.compare)
/// // -> Ok("c")
/// ```
pub fn max(
  over list: List(a),
  with compare: fn(a, a) -> Order,
) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] -> Ok(max_loop(rest, compare, first))
  }
}

fn max_loop(list, compare, max) {
  case list {
    [] -> max
    [first, ..rest] ->
      case compare(first, max) {
        order.Gt -> max_loop(rest, compare, first)
        order.Lt | order.Eq -> max_loop(rest, compare, max)
      }
  }
}

/// Returns a random sample of up to n elements from a list using reservoir
/// sampling via [Algorithm L](https://en.wikipedia.org/wiki/Reservoir_sampling#Optimal:_Algorithm_L).
/// Returns an empty list if the sample size is less than or equal to 0.
///
/// Order is not random, only selection is.
///
/// ## Examples
///
/// ```gleam
/// reservoir_sample([1, 2, 3, 4, 5], 3)
/// // -> [2, 4, 5]  // A random sample of 3 items
/// ```
///
pub fn sample(from list: List(a), up_to n: Int) -> List(a) {
  let #(reservoir, rest) = build_reservoir(from: list, sized: n)

  case dict.is_empty(reservoir) {
    // If the reservoire is empty that means we were asking to sample 0 or
    // less items. That doesn't make much sense, so we just return an empty
    // list.
    True -> []

    // Otherwise we keep looping over the remaining part of the list replacing
    // random elements in the reservoir.
    False -> {
      let w = float.exponential(log_random() /. int.to_float(n))
      dict.values(sample_loop(rest, reservoir, n, w))
    }
  }
}

fn sample_loop(
  list: List(a),
  reservoir: Dict(Int, a),
  n: Int,
  w: Float,
) -> Dict(Int, a) {
  let skip = {
    let assert Ok(log) = float.logarithm(1.0 -. w)
    float.round(float.floor(log_random() /. log))
  }

  case drop(list, skip) {
    [] -> reservoir
    [first, ..rest] -> {
      let reservoir = dict.insert(reservoir, int.random(n), first)
      let w = w *. float.exponential(log_random() /. int.to_float(n))
      sample_loop(rest, reservoir, n, w)
    }
  }
}

const min_positive = 2.2250738585072014e-308

fn log_random() -> Float {
  let assert Ok(random) = float.logarithm(float.random() +. min_positive)
  random
}

/// Builds the initial reservoir used by Algorithm L.
/// This is a dictionary with keys ranging from `0` up to `n - 1` where each
/// value is the corresponding element at that position in `list`.
///
/// This also returns the remaining elements of `list` that didn't end up in
/// the reservoir.
///
fn build_reservoir(from list: List(a), sized n: Int) -> #(Dict(Int, a), List(a)) {
  build_reservoir_loop(list, n, dict.new())
}

fn build_reservoir_loop(
  list: List(a),
  size: Int,
  reservoir: Dict(Int, a),
) -> #(Dict(Int, a), List(a)) {
  let reservoir_size = dict.size(reservoir)
  case reservoir_size >= size {
    // The reservoir already has the size we wanted.
    True -> #(reservoir, list)

    // Otherwise we add another element from the list to the reservoir
    False ->
      case list {
        [] -> #(reservoir, [])
        [first, ..rest] -> {
          let reservoir = dict.insert(reservoir, reservoir_size, first)
          build_reservoir_loop(rest, size, reservoir)
        }
      }
  }
}

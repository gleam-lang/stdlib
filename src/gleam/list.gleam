import gleam/int
import gleam/pair
import gleam/order.{Order}
import gleam/result.{Option}

pub type List(elements) =
  List(elements)

pub type LengthMismatch {
  LengthMismatch
}

/// Using the Erlang C BIF implementation.
///
pub external fn length(List(a)) -> Int = "erlang" "length"

/// Using the Erlang C BIF implementation.
///
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

pub fn is_empty(list: List(a)) -> Bool {
  list == []
}

pub fn contains(list: List(a), has elem: a) -> Bool {
  case list {
    [] -> False
    [head | rest] -> head == elem || contains(rest, elem)
  }
}

pub fn head(list: List(a)) -> Option(a) {
  case list {
    [] -> result.none()
    [x | _] -> Ok(x)
  }
}

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

pub fn filter(list: List(a), for predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}

fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x | xs] -> do_map(xs, fun, [fun(x) | acc])
  }
}

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

pub fn traverse(
  list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  do_traverse(list, fun, [])
}

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

pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  do_take(list, n, [])
}

pub fn new() -> List(a) {
  []
}

pub external fn append(List(a), List(a)) -> List(a)
  = "lists" "append";

fn do_flatten(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> acc
    [l | rest] -> do_flatten(rest, append(acc, l))
  }
}

pub fn flatten(lists: List(List(a))) -> List(a) {
  do_flatten(lists, [])
}

pub fn fold(list: List(a), from initial: b, with fun: fn(a, b) -> b) -> b {
  case list {
    [] -> initial
    [x | rest] -> fold(rest, fun(x, initial), fun)
  }
}

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

pub fn zip(xs: List(a), ys: List(b)) -> List(tuple(a, b)) {
  case xs, ys {
    [], _ -> []
    _, [] -> []
    [x | xs], [y | ys] -> [tuple(x, y) | zip(xs, ys)]
  }
}

pub fn strict_zip(l1: List(a), l2: List(b)) -> Result(List(tuple(a, b)), LengthMismatch) {
  case length(l1) == length(l2) {
    True -> Ok(zip(l1, l2))
    False -> Error(LengthMismatch)
  }
}

pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [x | rest] -> [x | [elem | intersperse(rest, elem)]]
  }
}

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

pub fn sort(list: List(a), sort_by compare: fn(a, a) -> Order) -> List(a) {
  do_sort(list, compare, length(list))
}

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

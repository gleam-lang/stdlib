import gleam/list

// Internal private representation of an Iterator
type Action(element) {
  Stop
  Continue(element, fn() -> Action(element))
}

/// An iterator is a lazily evaluated sequence of element.
///
/// Iterators are useful when working with collections that are too large to
/// fit in memory (or those that are infinite in size) as they only require the
/// elements currently being processed to be in memory.
///
/// As a lazy data structure no work is done when an iterator is filters,
/// mapped, etc, instead a new iterator is returned with these transformations
/// applied to the stream. Once the stream has all the required transformations
/// applied it can be evaluated using functions such as `fold` and `take`.
///
pub opaque type Iterator(element) {
  Iterator(continuation: fn() -> Action(element))
}

// Public API for iteration
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}

// Creating Iterators
fn do_unfold(
  initial: acc,
  f: fn(acc) -> Step(element, acc),
) -> fn() -> Action(element) {
  fn() {
    case f(initial) {
      Next(x, acc) -> Continue(x, do_unfold(acc, f))
      Done -> Stop
    }
  }
}

/// Create an iterator from a given function and accumulator.
///
/// The function is called on the accumulator and return either `Done`,
/// indicating the iterator has no more elements, or `Next` which contains a
/// new element and accumulator. The element is yielded by the iterator and the
/// new accumulator is used with the function to compute the next element in
/// the sequence.
///
/// ## Examples
///
///    > unfold(from: 5, with: fn(n) {
///    >  case n {
///    >    0 -> Done
///    >    n -> Next(element: n, accumulator: n - 1)
///    >  }
///    > })
///    > |> to_list
///    [5, 4, 3, 2, 1]
///
pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Iterator(element) {
  initial
  |> do_unfold(f)
  |> Iterator
}

// TODO: test
/// Create an iterator that yields values created by calling a given function
/// repeatedly.
///
pub fn repeatedly(f: fn() -> element) -> Iterator(element) {
  unfold(Nil, fn(_) { Next(f(), Nil) })
}

/// Create an iterator that returns the same value infinitely.
///
/// ## Examples
///
///    > repeat(10)
///    > |> take(4)
///    > |> to_list
///    [10, 10, 10, 10]
///
pub fn repeat(x: element) -> Iterator(element) {
  repeatedly(fn() { x })
}

/// Create an iterator the yields each element in a given list.
///
/// ## Examples
///
///    > from_list([1, 2, 3, 4]) |> to_list
///    [1, 2, 3, 4]
///
pub fn from_list(list: List(element)) -> Iterator(element) {
  let yield = fn(acc) {
    case acc {
      [] -> Done
      [head, ..tail] -> Next(head, tail)
    }
  }
  unfold(list, yield)
}

// Consuming Iterators
fn do_fold(
  continuation: fn() -> Action(e),
  initial: acc,
  f: fn(e, acc) -> acc,
) -> acc {
  case continuation() {
    Continue(element, iterator) -> do_fold(iterator, f(element, initial), f)
    Stop -> initial
  }
}

/// Reduce an iterator of elements into a single value by calling a given
/// function on each element in turn.
///
/// If called on an iterator of infinite length then this function will never
/// return.
///
/// If you do not care about the end value and only wish to evaluate the
/// iterator for side effects consider using the `run` function instead.
///
/// ## Examples
///
///    > [1, 2, 3, 4]
///    > |> from_list
///    > |> fold(from: 0, with: fn(element, acc) { element + acc })
///    10
///
pub fn fold(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(e, acc) -> acc,
) -> acc {
  iterator.continuation
  |> do_fold(initial, f)
}

// TODO: test
/// Evaluate all elements in a given stream. This function is useful for when
/// you wish to trigger any side effects that would occur when evaluating
/// the iterator.
///
pub fn run(iterator: Iterator(e)) -> Nil {
  fold(iterator, Nil, fn(_, _) { Nil })
}

/// Evaluate an iterator and return all the elements as a list.
///
/// If called on an iterator of infinite length then this function will never
/// return.
///
/// ## Examples
///
///   > [1, 2, 3] |> from_list |> map(fn(x) { x * 2 }) |> to_list
///   [2, 4, 6]
///
pub fn to_list(iterator: Iterator(element)) -> List(element) {
  iterator
  |> fold([], fn(e, acc) { [e, ..acc] })
  |> list.reverse
}

/// Eagerly access the first value of an interator, returning a `Next`
/// that contains the first value and the rest of the iterator.
///
/// If called on an empty iterator, `Done` is returned.
///
/// ## Examples
///
///    > assert Next(head, tail) =
///    >   [1, 2, 3, 4]
///    >   |> from_list
///    >   |> step
///    > head
///    1
///    > tail |> to_list
///    [2, 3, 4]
///
///    > []
///    > |> from_list
///    > |> step
///    Done
///
pub fn step(iterator: Iterator(e)) -> Step(e, Iterator(e)) {
  case iterator.continuation() {
    Stop -> Done
    Continue(e, a) -> Next(e, Iterator(a))
  }
}

fn do_take(
  continuation: fn() -> Action(e),
  desired: Int,
  acc: List(e),
) -> List(e) {
  case desired > 0 {
    True ->
      case continuation() {
        Continue(element, iterator) ->
          do_take(iterator, desired - 1, [element, ..acc])
        Stop ->
          acc
          |> list.reverse
      }
    False ->
      acc
      |> list.reverse
  }
}

/// Evaluate a desired number of elements from an iterator and return them in a
/// list.
///
/// If the iterator does not have enough elements all of them are returned.
///
/// ## Examples
///
///    > [1, 2, 3, 4, 5] |> from_list |> take(up_to: 3)
///    [1, 2, 3]
///
///    > [1, 2] |> from_list |> take(up_to: 3)
///    [1, 2]
///
pub fn take(from iterator: Iterator(e), up_to desired: Int) -> List(e) {
  iterator.continuation
  |> do_take(desired, [])
}

fn do_drop(continuation: fn() -> Action(e), desired: Int) -> fn() -> Action(e) {
  case desired > 0 {
    True ->
      case continuation() {
        Continue(_, iterator) -> do_drop(iterator, desired - 1)
        Stop -> fn() { Stop }
      }
    False -> continuation
  }
}

/// Evaluate and discard the first N elements in an iterator, returning a new
/// iterator.
///
/// If the iterator does not have enough elements an empty iterator is
/// returned.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
///    > [1, 2, 3, 4, 5] |> from_list |> drop(up_to: 3) |> to_list
///    [4, 5]
///
///    > [1, 2] |> from_list |> drop(up_to: 3) |> to_list
///    []
///
pub fn drop(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  iterator.continuation
  |> do_drop(desired)
  |> Iterator
}

fn do_map(continuation: fn() -> Action(a), f: fn(a) -> b) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Continue(e, continuation) -> Continue(f(e), do_map(continuation, f))
      Stop -> Stop
    }
  }
}

/// Create an iterator from an existing iterator and a transformation function.
///
/// Each element in the new iterator will be the result of calling the given
/// function on the elements in the given iterator.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
///    > [1, 2, 3] |> from_list |> map(fn(x) { x * 2 }) |> to_list
///    [2, 4, 6]
///
pub fn map(over iterator: Iterator(a), with f: fn(a) -> b) -> Iterator(b) {
  iterator.continuation
  |> do_map(f)
  |> Iterator
}

fn do_append(
  first: fn() -> Action(a),
  second: fn() -> Action(a),
) -> fn() -> Action(a) {
  fn() {
    case first() {
      Continue(e, first) -> Continue(e, do_append(first, second))
      Stop -> second()
    }
  }
}

/// Append two iterators, producing a new iterator.
///
/// This function does not evaluate the elements of the iterators, the
/// computation is performed when the resulting iterator is later run.
///
/// ## Examples
///
///    > [1, 2] |> from_list |> append([3, 4] |> from_list) |> to_list
///    [1, 2, 3, 4]
///
pub fn append(to first: Iterator(a), suffix second: Iterator(a)) -> Iterator(a) {
  first.continuation
  |> do_append(second.continuation)
  |> Iterator
}

fn do_flatten(continuation: fn() -> Action(Iterator(a))) -> fn() -> Action(a) {
  fn() {
    case continuation() {
      Continue(e, continuation) ->
        do_append(e.continuation, do_flatten(continuation))()
      Stop -> Stop
    }
  }
}

/// Flatten an iterator of iterator of iterators, creating a new iterator.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
///    > [[1, 2], [3, 4]] |> list.map(from_list) |> flatten |> to_list
///    [1, 2, 3, 4]
///
pub fn flatten(iterator: Iterator(Iterator(a))) -> Iterator(a) {
  iterator.continuation
  |> do_flatten
  |> Iterator
}

/// Create an iterator from an existing iterator and a transformation function.
///
/// Each element in the new iterator will be the result of calling the given
/// function on the elements in the given iterator and then flattening the
/// results.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
///    > [1, 2] |> from_list |> flat_map(fn(x) { from_list([x, x + 1]) }) |> to_list
///    [1, 2, 2, 3]
///
pub fn flat_map(
  over iterator: Iterator(a),
  with f: fn(a) -> Iterator(b),
) -> Iterator(b) {
  iterator
  |> map(f)
  |> flatten
}

fn do_filter(
  continuation: fn() -> Action(e),
  predicate: fn(e) -> Bool,
) -> fn() -> Action(e) {
  fn() {
    case continuation() {
      Continue(e, iterator) ->
        case predicate(e) {
          True -> Continue(e, do_filter(iterator, predicate))
          False -> do_filter(iterator, predicate)()
        }
      Stop -> Stop
    }
  }
}

/// Create an iterator from an existing iterator and a predicate function.
///
/// The new iterator will contain elements from the first iterator for which
/// the given function returns `True`.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
///    > import gleam/int
///    > [1, 2, 3, 4] |> from_list |> filter(int.is_even) |> to_list
///    [2, 4]
///
pub fn filter(
  iterator: Iterator(a),
  for predicate: fn(a) -> Bool,
) -> Iterator(a) {
  iterator.continuation
  |> do_filter(predicate)
  |> Iterator
}

fn do_cycle(next: fn() -> Action(a), reset: fn() -> Action(a)) {
  fn() {
    case next() {
      Continue(e, iterator) -> Continue(e, do_cycle(iterator, reset))
      Stop -> do_cycle(reset, reset)()
    }
  }
}

/// Create an iterator that repeats a given iterator infinitely.
///
/// ## Examples
///
///    > [1, 2] |> from_list |> cycle |> take(6)
///    [1, 2, 1, 2, 1, 2]
///
pub fn cycle(iterator: Iterator(a)) -> Iterator(a) {
  iterator.continuation
  |> do_cycle(iterator.continuation)
  |> Iterator
}

fn do_range(current, limit, inc) -> fn() -> Action(Int) {
  case current == limit {
    True -> fn() { Stop }
    False -> fn() { Continue(current, do_range(current + inc, limit, inc)) }
  }
}

/// Create an iterator of ints, starting at a given start int and stepping by
/// one to a given end int.
///
/// ## Examples
///
///    > range(from: 1, to: 5) |> to_list
///    [1, 2, 3, 4]
///
///    > range(from: 1, to: -2) |> to_list
///    [1, 0, -1]
///
///    > range(from: 0, to: 0) |> to_list
///    []
///
pub fn range(from start: Int, to stop: Int) -> Iterator(Int) {
  case start < stop {
    True -> 1
    False -> -1
  }
  |> do_range(start, stop, _)
  |> Iterator
}

/// Find the first element in a given iterator for which the given function returns
/// True.
///
/// Returns `Error(Nil)` if the function does not return True for any of the
/// elements.
///
/// ## Examples
///
///    > find(from_list([1, 2, 3]), fn(x) { x > 2 })
///    Ok(3)
///
///    > find(from_list([1, 2, 3]), fn(x) { x > 4 })
///    Error(Nil)
///
///    > find(from_list([]), fn(x) { True })
///    Error(Nil)
///
pub fn find(
  in haystack: Iterator(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  case haystack.continuation() {
    Continue(element, continuation) ->
      case is_desired(element) {
        True -> Ok(element)
        False -> find(Iterator(continuation), is_desired)
      }
    Stop -> Error(Nil)
  }
}

import gleam/list

// Internal private representation of an Iterator
type Action(element) {
  // Improper dancing in the middle of the street
  // Improper dancing in the middle of the street
  // Improper dancing in the middle of the street
  // Somebody better notify the chief of police
  Stop
  Continue(element, fn() -> Action(element))
}

// Yes!
// Wrapper to hide the internal representation
pub opaque type Iterator(element) {
  Iterator(thunk: fn() -> Action(element))
}

pub fn identity(x) {
  x
}

// Public API for iteration
pub type Step(element, acc) {
  Next(element, acc)
  Done
}

// Creating Iterators
fn do_unfold(initial, f) {
  fn() {
    case f(initial) {
      Next(x, acc) -> Continue(x, do_unfold(acc, f))
      Done -> Stop
    }
  }
}

// TODO: document
pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Iterator(element) {
  initial
  |> do_unfold(f)
  |> Iterator
}

// TODO: test
// TODO: document
pub fn repeatedly(f: fn() -> element) -> Iterator(element) {
  unfold(Nil, fn(acc) { Next(f(), acc) })
}

// TODO: document
pub fn repeat(x: element) -> Iterator(element) {
  repeatedly(fn() { x })
}

// TODO: document
pub fn from_list(list: List(element)) -> Iterator(element) {
  unfold(
    list,
    fn(acc) {
      case acc {
        [] -> Done
        [head, ..tail] -> Next(head, tail)
      }
    },
  )
}

// Consuming Iterators
fn do_fold(iterator, initial, f) {
  case iterator() {
    Continue(element, iterator) -> do_fold(iterator, f(element, initial), f)
    Stop -> initial
  }
}

// TODO: document
pub fn fold(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(e, acc) -> acc,
) -> acc {
  iterator.thunk
  |> do_fold(initial, f)
}

// TODO: test
// TODO: document
// For side effects
pub fn run(iterator) -> Nil {
  fold(iterator, Nil, fn(_, acc) { acc })
}

// TODO: document
pub fn to_list(iterator: Iterator(element)) -> List(element) {
  iterator
  |> fold([], fn(e, acc) { [e, ..acc] })
  |> list.reverse
}

fn do_take(iterator, desired, acc) {
  case desired > 0 {
    True -> case iterator() {
      Continue(
        element,
        iterator,
      ) -> do_take(iterator, desired - 1, [element, ..acc])
      Stop -> acc
    }
    False -> acc
  }
}

// TODO: document
pub fn take(from iterator: Iterator(e), up_to desired: Int) -> List(e) {
  iterator.thunk
  |> do_take(desired, [])
  |> list.reverse
}

fn do_drop(iterator, desired) {
  case desired > 0 {
    True -> case iterator() {
      Continue(_, iterator) -> do_drop(iterator, desired - 1)
      Stop -> fn() { Stop }
    }
    False -> iterator
  }
}

// TODO: document
pub fn drop(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  iterator.thunk
  |> do_drop(desired)
  |> Iterator
}

// Transforming Iterators
fn do_map(iterator, f) {
  fn() {
    case iterator() {
      Continue(e, iterator) -> Continue(f(e), do_map(iterator, f))
      Stop -> Stop
    }
  }
}

// TODO: document
pub fn map(over iterator: Iterator(a), with f: fn(a) -> b) -> Iterator(b) {
  iterator.thunk
  |> do_map(f)
  |> Iterator
}

fn do_filter(iterator, predicate) {
  fn() {
    case iterator() {
      Continue(e, iterator) -> case predicate(e) {
        True -> Continue(e, do_filter(iterator, predicate))
        False -> do_filter(iterator, predicate)()
      }
      Stop -> Stop
    }
  }
}

// TODO: document
pub fn filter(
  iterator: Iterator(a),
  for predicate: fn(a) -> Bool,
) -> Iterator(a) {
  iterator.thunk
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

// TODO: document
pub fn cycle(iterator: Iterator(a)) -> Iterator(a) {
  iterator.thunk
  |> do_cycle(iterator.thunk)
  |> Iterator
}

fn do_range(current, limit, inc) -> fn() -> Action(Int) {
  case current == limit {
    True -> fn() { Stop }
    False -> fn() { Continue(current, do_range(current + inc, limit, inc)) }
  }
}

// TODO: document
pub fn range(from start, to stop) -> Iterator(Int) {
  case start < stop {
    True -> 1
    False -> -1
  }
  |> do_range(start, stop, _)
  |> Iterator
}

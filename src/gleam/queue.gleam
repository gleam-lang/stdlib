import gleam/list

// TODO: document
pub opaque type Queue(element) {
  Queue(in: List(element), out: List(element))
}

// TODO: document
pub fn new() -> Queue(a) {
  Queue(in: [], out: [])
}

// TODO: document
pub fn from_list(list: List(a)) -> Queue(a) {
  Queue(in: list, out: [])
}

// TODO: document
pub fn to_list(queue: Queue(a)) -> List(a) {
  queue.in
  |> list.append(list.reverse(queue.out))
}

// TODO: document
pub fn is_empty(queue: Queue(a)) -> Bool {
  queue.in == [] && queue.out == []
}

// TODO: document
pub fn length(queue: Queue(a)) -> Int {
  list.length(queue.in) + list.length(queue.out)
}

// TODO: document
pub fn push_back(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: [item, ..queue.in], out: queue.out)
}

// TODO: document
pub fn push_front(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: queue.in, out: [item, ..queue.out])
}

// TODO: document
pub fn pop_back(from queue: Queue(a)) -> Result(tuple(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: in, out: []) -> pop_back(Queue(in: [], out: list.reverse(in)))
    Queue(in: in, out: [first, ..rest]) -> {
      let queue = Queue(in: in, out: rest)
      Ok(tuple(first, queue))
    }
  }
}

// TODO: document
pub fn pop_front(from queue: Queue(a)) -> Result(tuple(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: [], out: out) -> pop_front(Queue(in: list.reverse(out), out: []))
    Queue(in: [first, ..rest], out: out) -> {
      let queue = Queue(in: rest, out: out)
      Ok(tuple(first, queue))
    }
  }
}

// TODO: document
// O(1)
pub fn reverse(queue: Queue(a)) -> Queue(a) {
  Queue(in: queue.out, out: queue.in)
}

fn check_equal(
  xs: List(t),
  x_tail: List(t),
  ys: List(t),
  y_tail: List(t),
  eq: fn(t, t) -> Bool,
) -> Bool {
  case xs, x_tail, ys, y_tail {
    [], [], [], [] -> True
    [x, ..xs], _, [y, ..ys], _ -> case eq(x, y) {
      False -> False
      True -> check_equal(xs, x_tail, ys, y_tail, eq)
    }
    [], [_, ..], _, _ -> check_equal(list.reverse(x_tail), [], ys, y_tail, eq)
    _, _, [], [_, ..] -> check_equal(xs, x_tail, list.reverse(y_tail), [], eq)
    _, _, _, _ -> False
  }
}

// TODO: document
pub fn is_logically_equal(
  a: Queue(t),
  to b: Queue(t),
  checking element_is_equal: fn(t, t) -> Bool,
) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, element_is_equal)
}

// TODO: document
pub fn is_equal(a: Queue(t), to b: Queue(t)) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, fn(a, b) { a == b })
}

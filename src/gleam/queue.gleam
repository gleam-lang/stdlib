import gleam/list

/// A queue is an order collection of elements. It is similar to a list, but
/// unlike a list elements can be added to or removed from either the front or
/// the back in a performant fashion.
///
/// The internal representation may be different for two queues with the same
/// elements in the same order if the queues were constructed in different
/// ways. This is the price paid for a queue's fast access at both the front
/// and the back.
///
/// Because of unpredictable internal representation the equality operator `==`
/// may return surprising results, and the `is_equal` and `is_logically_equal`
/// functions are the recommended way to test queues for equality.
///
pub opaque type Queue(element) {
  Queue(in: List(element), out: List(element))
}

/// Create a fresh queue that contains no values.
///
pub fn new() -> Queue(a) {
  Queue(in: [], out: [])
}

/// Convert a list of elements into a queue of the same elements in the same
/// order.
///
/// This function runs in constant time.
///
/// # Examples
///
///    > [1, 2, 3] |> from_list |> length
///    3
///
pub fn from_list(list: List(a)) -> Queue(a) {
  Queue(in: list, out: [])
}

/// Convert a queue of elements into a list of the same elements in the same
/// order.
///
/// This function runs in linear time.
///
/// # Examples
///
///    > new() |> push_back(1) |> push_back(2) |> to_list
///    [1, 2]
///
pub fn to_list(queue: Queue(a)) -> List(a) {
  queue.out
  |> list.append(list.reverse(queue.in))
}

/// Determine whether or not the queue is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > [] |> from_list |> is_empty
///    True
///
///    > [1] |> from_list |> is_empty
///    False
///
///    > [1, 2] |> from_list |> is_empty
///    False
///
pub fn is_empty(queue: Queue(a)) -> Bool {
  queue.in == [] && queue.out == []
}

/// Count the number of elements in a given queue.
///
/// This function has to traverse the queue to determine the number of elements,
/// so it runs in linear time.
///
/// ## Examples
///
///    > length(from_list([]))
///    0
///
///    > length(from_list([1]))
///    1
///
///    > length(from_list([1, 2]))
///    2
///
pub fn length(queue: Queue(a)) -> Int {
  list.length(queue.in) + list.length(queue.out)
}

/// Push an element onto the back of the queue.
///
/// # Examples
///
///    > [0, 0] |> from_list |> push_back(1) |> to_list
///    [0, 0, 1]
///
pub fn push_back(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: [item, ..queue.in], out: queue.out)
}

/// Push an element onto the front of the queue.
///
/// # Examples
///
///    > [0, 0] |> from_list |> push_front(1) |> to_list
///    [1, 0, 0]
///
pub fn push_front(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: queue.in, out: [item, ..queue.out])
}

/// Get the first element from the back of the of the queue, returning the
/// element and a new queue without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
///    > queue.new()
///    > |> queue.push_front(0)
///    > |> queue.push_back(1)
///    > |> queue.pop_back()
///    Ok(tuple(1, queue.push_front(queue.new(), 0)))
///
///    > queue.new()
///    > |> queue.push_front(0)
///    > |> queue.pop_back()
///    Ok(tuple(0, queue.new()))
///
///    > queue.new()
///    > |> queue.pop_back()
///    Error(Nil)
///
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

/// Get the first element from the front of the of the queue, returning the
/// element and a new queue without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
///    > queue.new()
///    > |> queue.push_front(0)
///    > |> queue.push_back(1)
///    > |> queue.pop_front()
///    Ok(tuple(0, queue.push_back(queue.new(), 1)))
///
///    > queue.new()
///    > |> queue.push_back(0)
///    > |> queue.pop_front()
///    Ok(tuple(0, queue.new()))
///
///    > queue.new()
///    > |> queue.pop_back()
///    Error(Nil)
///
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

/// Create a new queue from a given queue containing the same elements, but in
/// the opposite order.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > reverse(from_list([]))
///    []
///
///    > reverse(from_list([1]))
///    [1]
///
///    > reverse(from_list([1, 2]))
///    [2, 1]
///
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
    [x, ..xs], _, [y, ..ys], _ ->
      case eq(x, y) {
        False -> False
        True -> check_equal(xs, x_tail, ys, y_tail, eq)
      }
    [], [_, ..], _, _ -> check_equal(list.reverse(x_tail), [], ys, y_tail, eq)
    _, _, [], [_, ..] -> check_equal(xs, x_tail, list.reverse(y_tail), [], eq)
    _, _, _, _ -> False
  }
}

/// Check whether two queues have equal elements in the same order, where the
/// equality of elements is determined by a given equality checking function.
///
/// This function is useful as the internal representation may be different for
/// two queues with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time multiplied by the time taken by the
/// element equality checking function.
///
pub fn is_logically_equal(
  a: Queue(t),
  to b: Queue(t),
  checking element_is_equal: fn(t, t) -> Bool,
) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, element_is_equal)
}

/// Check whether two queues have the same elements in the same order.
///
/// This function is useful as the internal representation may be different for
/// two queues with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time.
///
pub fn is_equal(a: Queue(t), to b: Queue(t)) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, fn(a, b) { a == b })
}

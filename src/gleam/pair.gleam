/// Returns the first element in a pair.
///
/// ## Examples
///
///    > first(tuple(1, 2))
///    1
///
pub fn first(pair: tuple(a, b)) -> a {
  let tuple(a, _) = pair
  a
}

/// Returns the second element in a pair.
///
/// ## Examples
///
///    > second(tuple(1, 2))
///    2
///
pub fn second(pair: tuple(a, b)) -> b {
  let tuple(_, a) = pair
  a
}

/// Returns a new pair with the elements swapped.
///
/// ## Examples
///
///    > swap(tuple(1, 2))
///    tuple(2, 1)
///
pub fn swap(pair: tuple(a, b)) -> tuple(b, a) {
  let tuple(a, b) = pair
  tuple(b, a)
}

/// Returns a new pair with the first element having had `with` applied to
/// it.
///
/// ## Examples
///
///    > tuple(1, 2) |> map_first(fn(n) { n * 2 })
///    2
///
pub fn map_first(of pair: tuple(a, b), with fun: fn(a) -> c) -> tuple(c, b) {
  let tuple(a, b) = pair
  tuple(fun(a), b)
}

/// Returns a new pair with the second element having had `with` applied to
/// it.
///
/// ## Examples
///
///    > tuple(1, 2) |> map_second(fn(n) { n * 2 })
///    4
///
pub fn map_second(of pair: tuple(a, b), with fun: fn(b) -> c) -> tuple(a, c) {
  let tuple(a, b) = pair
  tuple(a, fun(b))
}

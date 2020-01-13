pub fn first(pair: tuple(a, b)) -> a {
  let tuple(a, _) = pair
  a
}

pub fn second(pair: tuple(a, b)) -> b {
  let tuple(_, a) = pair
  a
}

pub fn swap(pair: tuple(a, b)) -> tuple(b, a) {
  let tuple(a, b) = pair
  tuple(b, a)
}

pub fn map_first(of pair: tuple(a, b), with fun: fn(a) -> c) -> tuple(c, b) {
  let tuple(a, b) = pair
  tuple(fun(a), b)
}

pub fn map_second(of pair: tuple(a, b), with fun: fn(b) -> c) -> tuple(a, c) {
  let tuple(a, b) = pair
  tuple(a, fun(b))
}

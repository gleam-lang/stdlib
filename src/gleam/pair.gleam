pub fn first(pair) {
  let tuple(a, _) = pair
  a
}

pub fn second(pair) {
  let tuple(_, a) = pair
  a
}

pub fn swap(pair) {
  let tuple(a, b) = pair
  tuple(b, a)
}

pub fn map_first(of pair, with fun) {
  let tuple(a, b) = pair
  tuple(fun(a), b)
}

pub fn map_second(of pair, with fun) {
  let tuple(a, b) = pair
  tuple(a, fun(b))
}

pub fn first(pair) {
  let struct(a, _) = pair
  a
}

pub fn second(pair) {
  let struct(_, a) = pair
  a
}

pub fn swap(pair) {
  let struct(a, b) = pair
  struct(b, a)
}

pub fn map_first(of pair, with fun) {
    let struct(a, b) = pair
    struct(fun(a), b)
}

pub fn map_second(of pair, with fun) {
    let struct(a, b) = pair
    struct(a, fun(b))
}

pub type Order {
  Lt
  Eq
  Gt
}

pub fn reverse(order: Order) -> Order {
  case order {
    Lt -> Gt
    Eq -> Eq
    Gt -> Lt
  }
}

pub fn to_int(order: Order) -> Int {
  case order {
    Lt -> -1
    Eq -> 0
    Gt -> 1
  }
}

pub fn compare(a: Order, b: Order) -> Order {
  case a, b {
    x, y if x == y -> Eq
    Lt, _ | Eq, Gt -> Lt
    _, _ -> Gt
  }
}

pub fn max(a: Order, b: Order) -> Order {
  case a, b {
    Gt, _ -> Gt
    Eq, Lt -> Eq
    _, _ -> b
  }
}

pub fn min(a: Order, b: Order) -> Order {
  case a, b {
    Lt, _ -> Lt
    Eq, Gt -> Eq
    _, _ -> b
  }
}

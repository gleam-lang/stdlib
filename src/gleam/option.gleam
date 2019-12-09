pub enum Option(a) {
  Some(a)
  None
}

pub fn is_some(option) {
  case option {
    None -> False
    Some(_) -> True
  }
}

pub fn is_none(option) {
  case option {
    Some(_) -> False
    None -> True
  }
}

pub fn map(option, with fun) {
  case option {
    Some(x) -> Some(fun(x))
    None -> None
  }
}

pub fn flatten(option) {
  case option {
    Some(x) -> x
    None -> None
  }
}

pub fn then(option, apply fun) {
  case option {
    Some(x) -> fun(x)
    None -> None
  }
}

pub fn unwrap(option, or default) {
  case option {
    Some(v) -> v
    None -> default
  }
}

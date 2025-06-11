import gleam/bool

pub fn and_test() {
  assert bool.and(True, True)

  assert !bool.and(False, True)

  assert bool.and(True, True)

  assert !bool.and(True, False)
}

pub fn or_test() {
  assert bool.or(True, True)

  assert bool.or(False, True)

  assert bool.or(True, False)

  assert bool.or(True, False)
}

pub fn negate_test() {
  assert !bool.negate(True)

  assert bool.negate(False)
}

pub fn nor_test() {
  assert bool.nor(False, False)

  assert !bool.nor(False, True)

  assert !bool.nor(True, False)

  assert !bool.nor(True, True)
}

pub fn nand_test() {
  assert bool.nand(False, False)

  assert bool.nand(False, True)

  assert bool.nand(True, False)

  assert !bool.nand(True, True)
}

pub fn exclusive_or_test() {
  assert !bool.exclusive_or(True, True)

  assert !bool.exclusive_or(False, False)

  assert bool.exclusive_or(True, False)

  assert bool.exclusive_or(False, True)
}

pub fn exclusive_nor_test() {
  assert bool.exclusive_nor(False, False)

  assert !bool.exclusive_nor(False, True)

  assert !bool.exclusive_nor(True, False)

  assert bool.exclusive_nor(True, True)
}

pub fn to_string_test() {
  assert bool.to_string(True) == "True"

  assert bool.to_string(False) == "False"
}

pub fn guard_test() {
  let assert 2 = {
    use <- bool.guard(when: True, return: 2)
    1
  }

  let assert 1 = {
    use <- bool.guard(when: False, return: 2)
    1
  }
}

pub fn lazy_guard_test() {
  let oops = fn() { panic as "this shouldn't run!" }

  let assert 2 = {
    use <- bool.lazy_guard(when: True, otherwise: oops)
    2
  }

  let assert 1 = {
    use <- bool.lazy_guard(when: False, return: oops)
    1
  }
}

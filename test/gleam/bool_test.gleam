import gleam/bool

pub fn and_true_true_test() {
  assert bool.and(True, True)
}

pub fn and_false_true_test() {
  assert !bool.and(False, True)
}

pub fn and_true_true_2_test() {
  assert bool.and(True, True)
}

pub fn and_true_false_test() {
  assert !bool.and(True, False)
}

pub fn or_true_true_test() {
  assert bool.or(True, True)
}

pub fn or_false_true_test() {
  assert bool.or(False, True)
}

pub fn or_true_false_test() {
  assert bool.or(True, False)
}

pub fn or_true_false_2_test() {
  assert bool.or(True, False)
}

pub fn negate_true_test() {
  assert !bool.negate(True)
}

pub fn negate_false_test() {
  assert bool.negate(False)
}

pub fn nor_false_false_test() {
  assert bool.nor(False, False)
}

pub fn nor_false_true_test() {
  assert !bool.nor(False, True)
}

pub fn nor_true_false_test() {
  assert !bool.nor(True, False)
}

pub fn nor_true_true_test() {
  assert !bool.nor(True, True)
}

pub fn nand_false_false_test() {
  assert bool.nand(False, False)
}

pub fn nand_false_true_test() {
  assert bool.nand(False, True)
}

pub fn nand_true_false_test() {
  assert bool.nand(True, False)
}

pub fn nand_true_true_test() {
  assert !bool.nand(True, True)
}

pub fn exclusive_or_true_true_test() {
  assert !bool.exclusive_or(True, True)
}

pub fn exclusive_or_false_false_test() {
  assert !bool.exclusive_or(False, False)
}

pub fn exclusive_or_true_false_test() {
  assert bool.exclusive_or(True, False)
}

pub fn exclusive_or_false_true_test() {
  assert bool.exclusive_or(False, True)
}

pub fn exclusive_nor_false_false_test() {
  assert bool.exclusive_nor(False, False)
}

pub fn exclusive_nor_false_true_test() {
  assert !bool.exclusive_nor(False, True)
}

pub fn exclusive_nor_true_false_test() {
  assert !bool.exclusive_nor(True, False)
}

pub fn exclusive_nor_true_true_test() {
  assert bool.exclusive_nor(True, True)
}

pub fn to_string_true_test() {
  assert bool.to_string(True) == "True"
}

pub fn to_string_false_test() {
  assert bool.to_string(False) == "False"
}

pub fn guard_when_true_test() {
  let assert 2 = {
    use <- bool.guard(when: True, return: 2)
    1
  }
}

pub fn guard_when_false_test() {
  let assert 1 = {
    use <- bool.guard(when: False, return: 2)
    1
  }
}

pub fn lazy_guard_when_true_test() {
  let oops = fn() { panic as "this shouldn't run!" }

  let assert 2 = {
    use <- bool.lazy_guard(when: True, otherwise: oops)
    2
  }
}

pub fn lazy_guard_when_false_test() {
  let oops = fn() { panic as "this shouldn't run!" }

  let assert 1 = {
    use <- bool.lazy_guard(when: False, return: oops)
    1
  }
}

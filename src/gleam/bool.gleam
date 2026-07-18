//// A type with two possible values, `True` and `False`. Used to indicate whether
//// things are... true or false!
////
//// It is often clearer and offers more type safety to define a custom type
//// than to use `Bool`. For example, rather than having a `is_teacher: Bool`
//// field consider having a `role: SchoolRole` field where `SchoolRole` is a custom
//// type that can be either `Student` or `Teacher`.

/// Returns the and of two bools, but it evaluates both arguments.
///
/// It's the function equivalent of the `&&` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// assert bool.and(True, True)
/// ```
///
/// ```gleam
/// assert !bool.and(False, True)
/// ```
///
/// ```gleam
/// assert !bool.and(False, True)
/// ```
///
/// ```gleam
/// assert !bool.and(False, False)
/// ```
///
pub fn and(a: Bool, b: Bool) -> Bool {
  a && b
}

/// Returns the or of two bools, but it evaluates both arguments.
///
/// It's the function equivalent of the `||` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// assert bool.or(True, True)
/// ```
///
/// ```gleam
/// assert bool.or(False, True)
/// ```
///
/// ```gleam
/// assert bool.or(True, False)
/// ```
///
/// ```gleam
/// assert !bool.or(False, False)
/// ```
///
pub fn or(a: Bool, b: Bool) -> Bool {
  a || b
}

/// Returns the opposite bool value.
///
/// This is the same as the `!` or `not` operators in some other languages.
///
/// ## Examples
///
/// ```gleam
/// assert !bool.negate(True)
/// ```
///
/// ```gleam
/// assert bool.negate(False)
/// ```
///
pub fn negate(bool: Bool) -> Bool {
  !bool
}

/// Returns the nor of two bools.
///
/// ## Examples
///
/// ```gleam
/// assert bool.nor(False, False)
/// ```
///
/// ```gleam
/// assert !bool.nor(False, True)
/// ```
///
/// ```gleam
/// assert !bool.nor(True, False)
/// ```
///
/// ```gleam
/// assert !bool.nor(True, True)
/// ```
///
pub fn nor(a: Bool, b: Bool) -> Bool {
  !{ a || b }
}

/// Returns the nand of two bools.
///
/// ## Examples
///
/// ```gleam
/// assert bool.nand(False, False)
/// ```
///
/// ```gleam
/// assert bool.nand(False, True)
/// ```
///
/// ```gleam
/// assert bool.nand(True, False)
/// ```
///
/// ```gleam
/// assert !bool.nand(True, True)
/// ```
///
pub fn nand(a: Bool, b: Bool) -> Bool {
  !{ a && b }
}

/// Returns the exclusive or of two bools.
///
/// ## Examples
///
/// ```gleam
/// assert !bool.exclusive_or(False, False)
/// ```
///
/// ```gleam
/// assert bool.exclusive_or(False, True)
/// ```
///
/// ```gleam
/// assert bool.exclusive_or(True, False)
/// ```
///
/// ```gleam
/// assert !bool.exclusive_or(True, True)
/// ```
///
pub fn exclusive_or(a: Bool, b: Bool) -> Bool {
  a != b
}

/// Returns the exclusive nor of two bools.
///
/// ## Examples
///
/// ```gleam
/// assert bool.exclusive_nor(False, False)
/// ```
///
/// ```gleam
/// assert !bool.exclusive_nor(False, True)
/// ```
///
/// ```gleam
/// assert !bool.exclusive_nor(True, False)
/// ```
///
/// ```gleam
/// assert bool.exclusive_nor(True, True)
/// ```
///
pub fn exclusive_nor(a: Bool, b: Bool) -> Bool {
  a == b
}

/// Returns a string representation of the given bool.
///
/// ## Examples
///
/// ```gleam
/// assert bool.to_string(True) == "True"
/// ```
///
/// ```gleam
/// assert bool.to_string(False) == "False"
/// ```
///
pub fn to_string(bool: Bool) -> String {
  case bool {
    False -> "False"
    True -> "True"
  }
}

/// Run a callback function if the given bool is `False`, otherwise return a
/// default value.
///
/// With a `use` expression this function can simulate the early-return pattern
/// found in some other programming languages.
///
/// In a procedural language:
///
/// ```js
/// if (predicate) return value;
/// // ...
/// ```
///
/// In Gleam with a `use` expression:
///
/// ```gleam
/// use <- bool.guard(when: predicate, return: value)
/// todo
/// // ...
/// ```
///
/// Like everything in Gleam `use` is an expression, so it short circuits the
/// current block, not the entire function. As a result you can assign the value
/// to a variable:
///
/// ```gleam
/// let x = {
///   use <- bool.guard(when: predicate, return: value)
///   todo
///   // ...
/// }
/// ```
///
/// Note that unlike in procedural languages the `return` value is evaluated
/// even when the predicate is `False`, so it is advisable not to perform
/// expensive computation nor side-effects there.
///
///
/// ## Examples
///
/// ```gleam
/// let name = ""
/// use <- bool.guard(when: name == "", return: "Welcome!")
/// "Hello, " <> name
/// // -> "Welcome!"
/// ```
///
/// ```gleam
/// let name = "Kamaka"
/// use <- bool.guard(when: name == "", return: "Welcome!")
/// "Hello, " <> name
/// // -> "Hello, Kamaka"
/// ```
///
pub fn guard(
  when requirement: Bool,
  return consequence: a,
  otherwise alternative: fn() -> a,
) -> a {
  case requirement {
    True -> consequence
    False -> alternative()
  }
}

/// Runs a callback function if the given bool is `True`, otherwise runs an
/// alternative callback function.
///
/// Useful when further computation should be delayed regardless of the given
/// bool's value.
///
/// See [`guard`](#guard) for more info.
///
/// ## Examples
///
/// ```gleam
/// let name = "Kamaka"
/// let inquiry = fn() { "How may we address you?" }
/// use <- bool.lazy_guard(when: name == "", return: inquiry)
/// "Hello, " <> name
/// // -> "Hello, Kamaka"
/// ```
///
/// ```gleam
/// import gleam/int
///
/// let name = ""
/// let greeting = fn() { "Hello, " <> name }
/// use <- bool.lazy_guard(when: name == "", otherwise: greeting)
/// let number = int.random(99)
/// let name = "User " <> int.to_string(number)
/// "Welcome, " <> name
/// // -> "Welcome, User 54"
/// ```
///
pub fn lazy_guard(
  when requirement: Bool,
  return consequence: fn() -> a,
  otherwise alternative: fn() -> a,
) -> a {
  case requirement {
    True -> consequence()
    False -> alternative()
  }
}

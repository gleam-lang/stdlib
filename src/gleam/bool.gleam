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
/// assert and(True, True)
/// ```
///
/// ```gleam
/// assert !and(False, True)
/// ```
///
/// ```gleam
/// assert !and(False, True)
/// ```
///
/// ```gleam
/// assert !and(False, False)
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
/// assert or(True, True)
/// ```
///
/// ```gleam
/// assert or(False, True)
/// ```
///
/// ```gleam
/// assert or(True, False)
/// ```
///
/// ```gleam
/// assert !or(False, False)
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
/// assert !negate(True)
/// ```
///
/// ```gleam
/// assert negate(False)
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
/// assert nor(False, False)
/// ```
///
/// ```gleam
/// assert !nor(False, True)
/// ```
///
/// ```gleam
/// assert !nor(True, False)
/// ```
///
/// ```gleam
/// assert !nor(True, True)
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
/// assert nand(False, False)
/// ```
///
/// ```gleam
/// assert nand(False, True)
/// ```
///
/// ```gleam
/// assert nand(True, False)
/// ```
///
/// ```gleam
/// assert !nand(True, True)
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
/// assert !exclusive_or(False, False)
/// ```
///
/// ```gleam
/// assert exclusive_or(False, True)
/// ```
///
/// ```gleam
/// assert exclusive_or(True, False)
/// ```
///
/// ```gleam
/// assert !exclusive_or(True, True)
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
/// assert exclusive_nor(False, False)
/// ```
///
/// ```gleam
/// assert !exclusive_nor(False, True)
/// ```
///
/// ```gleam
/// assert !exclusive_nor(True, False)
/// ```
///
/// ```gleam
/// assert exclusive_nor(True, True)
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
/// assert to_string(True) == "True"
/// ```
///
/// ```gleam
/// assert to_string(False) == "False"
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
/// use <- guard(when: predicate, return: value)
/// // ...
/// ```
///
/// Like everything in Gleam `use` is an expression, so it short circuits the
/// current block, not the entire function. As a result you can assign the value
/// to a variable:
///
/// ```gleam
/// let x = {
///   use <- guard(when: predicate, return: value)
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
/// use <- guard(when: name == "", return: "Welcome!")
/// "Hello, " <> name
/// // -> "Welcome!"
/// ```
///
/// ```gleam
/// let name = "Kamaka"
/// use <- guard(when: name == "", return: "Welcome!")
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
/// use <- lazy_guard(when: name == "", return: inquiry)
/// "Hello, " <> name
/// // -> "Hello, Kamaka"
/// ```
///
/// ```gleam
/// import gleam/int
///
/// let name = ""
/// let greeting = fn() { "Hello, " <> name }
/// use <- lazy_guard(when: name == "", otherwise: greeting)
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

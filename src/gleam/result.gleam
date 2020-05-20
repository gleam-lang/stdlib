/// Result represents the result of something that may succeed or not.
/// `Ok` means it was successful, `Error` means it was not successful.
///
pub type Result(success, error) =
  Result(success, error)

/// Nil is a type used to represent the absence of something, similar to null
/// or undefined in other languages.
///
/// Unlike some other languages values cannot be implicitly nil, value that may
/// be absent is typically represented using `Result(TheType, Nil)`. This is
/// such a common type that offer the `Option(TheType)` alias.
///
pub type Nil =
  Nil

/// A value that is either there or not there.
///
/// Some other languages have a dedicated Option type that is not related to
/// Result for this, however this tends to have all the same functions as
/// Result so in Gleam we combine the two.
///
pub type Option(value) =
  Result(value, Nil)

/// Check whether the result is an Ok value.
///
/// ## Examples
///
///    > is_ok(Ok(1))
///    True
///
///    > is_ok(Error(Nil))
///    False
///
pub fn is_ok(result: Result(a, e)) -> Bool {
  case result {
    Error(_) -> False
    Ok(_) -> True
  }
}

/// Check whether the result is an Error value.
///
/// ## Examples
///
///    > is_error(Ok(1))
///    False
///
///    > is_error(Error(Nil))
///    True
///
pub fn is_error(result: Result(a, e)) -> Bool {
  case result {
    Ok(_) -> False
    Error(_) -> True
  }
}

/// Update a value held within the Ok of a result by calling a given function
/// on it.
///
/// If the result is an Error rather than OK the function is not called and the
/// result stays the same.
///
/// ## Examples
///
///    > map(over: Ok(1), with: fn(x) { x + 1 })
///    Ok(2)
///
///    > map(over: Error(1), with: fn(x) { x + 1 })
///    Error(1)
///
pub fn map(over result: Result(a, e), with fun: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(x) -> Ok(fun(x))
    Error(e) -> Error(e)
  }
}

/// Update a value held within the Error of a result by calling a given function
/// on it.
///
/// If the result is Ok rather than Error the function is not called and the
/// result stays the same.
///
/// ## Examples
///
///    > map_error(over: Error(1), with: fn(x) { x + 1 })
///    Error(2)
///
///    > map_error(over: Ok(1), with: fn(x) { x + 1 })
///    Ok(1)
///
pub fn map_error(
  over result: Result(a, e),
  with fun: fn(e) -> f,
) -> Result(a, f) {
  case result {
    Ok(x) -> Ok(x)
    Error(error) -> Error(fun(error))
  }
}

/// Merge a nested Result into a single layer.
///
/// ## Examples
///
///    > flatten(Ok(Ok(1)))
///    Ok(1)
///
///    > flatten(Ok(Error(""))
///    Error("")
///
///    > flatten(Error(Nil))
///    Error(Nil)
///
pub fn flatten(result: Result(Result(a, e), e)) -> Result(a, e) {
  case result {
    Ok(x) -> x
    Error(error) -> Error(error)
  }
}

/// Update a value held within the Ok of a result by calling a given function
/// on it, where the given function also returns a result. The two results are
/// then merged together into one result.
///
/// If the result is an Error rather than OK the function is not called and the
/// result stays the same.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that may fail.
///
/// ## Examples
///
///    > then(Ok(1), fn(x) { Ok(x + 1) })
///    Ok(2)
///
///    > then(Ok(1), fn(x) { Ok(tuple("a", x)) })
///    Ok(tuple("a", 1))
///
///    > then(Ok(1), fn(x) { Error("Oh no") })
///    Error("Oh no")
///
///    > then(Error(Nil), fn(x) { Ok(x + 1) })
///    Error(Nil)
///
pub fn then(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(x) -> fun(x)
    Error(e) -> Error(e)
  }
}

/// Extract the Ok value from a result, returning a default value if the result
/// is an Error.
///
/// ## Examples
///
///    > unwrap(Ok(1), 0)
///    1
///
///    > unwrap(Error(""), 0)
///    0
///
pub fn unwrap(result: Result(a, e), or default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Another way of writing `Error(Nil)`.
///
/// ## Examples
///
///    > none()
///    Error(Nil)
///
pub fn none() -> Option(a) {
  Error(Nil)
}

/// Transforms any error into Error(Nil)
///
/// ## Examples
///
///    > nil_error(Error(1))
///    Error(Nil)
///
///    > nil_error(Ok(1))
///    Ok(1)
///
pub fn nil_error(result: Result(a, e)) -> Result(a, Nil) {
  map_error(result, fn(_) { Nil })
}

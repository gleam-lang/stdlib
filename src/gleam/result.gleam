/// Result represents the result of something that may succeed or fail.
/// `Ok` means it was successful, `Error` means it failed.
pub type Result(success, error) =
  Result(success, error)

/// Nil is a type used to represent the absence of something, similar to null
/// or undefined in other languages.
///
/// Unlike some other languages values cannot be implicitly nil, value that may
/// be absent is typically represented using `Result(TheType, Nil)`. This is
/// such a common type that offer the `Option(TheType)` alias.
pub type Nil =
  Nil

/// A value that is either there or not there.
pub type Option(value) =
  Result(value, Nil)

/// Returns whether the value is Ok
///
pub fn is_ok(result: Result(a, e)) -> Bool {
  case result {
    Error(_) -> False
    Ok(_) -> True
  }
}

/// Returns whether the value is Error
pub fn is_error(result: Result(a, e)) -> Bool {
  case result {
    Ok(_) -> False
    Error(_) -> True
  }
}

/// Executes the function `with` on inner value when Result is Ok, will noop
/// if it is Error
///
pub fn map(
  over result: Result(a, e),
  with fun: fn(a) -> b,
) -> Result(b, e) {
  case result {
    Ok(x) -> Ok(fun(x))
    Error(e) -> Error(e)
  }
}

/// Will execute the function `with` on inner value  when Result is Err, will noop
/// if it is Ok
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

/// Will unnest the inner value of a result nested within another result
///
pub fn flatten(result: Result(Result(a, e), e)) -> Result(a, e) {
  case result {
    Ok(x) -> x
    Error(error) -> Error(error)
  }
}

/// An alias of `map`
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

/// Will return the inner value of a Ok value.  If an error, will
/// return the value provided as `or`
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
///
pub fn none() -> Option(a) {
  Error(Nil)
}

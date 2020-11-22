/// Option represents a value that may be present or not. Some means the value is
/// present, None means the value is not.
///
/// This is Gleam's alternative to having a value that could be Null, as is
/// possible in some other languages.
///
pub type Option(a) {
  Some(a)
  None
}

/// Check whether the option is a Some value.
///
/// ## Examples
///
///    > is_some(Some(1))
///    True
///
///    > is_some(None)
///    False
///
pub fn is_some(option: Option(a)) -> Bool {
  option != None
}

/// Check whether the option is a None value.
///
/// ## Examples
///
///    > is_none(Some(1))
///    False
///
///    > is_none(None)
///    True
///
pub fn is_none(option: Option(a)) -> Bool {
  option == None
}

/// Converts an Option type to a Result type
///
/// ## Examples
///
///    > to_result(Some(1), "some_error")
///    Ok(1)
///    > to_result(None, "some_error")
///    Error("some_error")
///
pub fn to_result(option: Option(a), e) -> Result(a, e) {
  case option {
    Some(a) -> Ok(a)
    _ -> Error(e)
  }
}

/// Converts a Result type to an Option type
///
/// ## Examples
///
///    > from_result(Ok(1))
///    Some(1)
///    > from_result(Error"some_error"))
///    None
///
pub fn from_result(result: Result(a, e)) -> Option(a) {
  case result {
    Ok(a) -> Some(a)
    _ -> None
  }
}

/// Extract the value from an option, returning a default value if there is none.
///
/// ## Examples
///
///    > unwrap(Some(1), 0)
///    1
///
///    > unwrap(None, 0)
///    0
///
pub fn unwrap(option: Option(a), or default: a) -> a {
  case option {
    Some(x) -> x
    None -> default
  }
}

/// Update a value held within the Some of an Option by calling a given function
/// on it.
///
/// If the option is a None rather than Some the function is not called and the
/// option stays the same.
///
/// ## Examples
///
///    > map(over: Some(1), with: fn(x) { x + 1 })
///    Some(2)
///
///    > map(over: None, with: fn(x) { x + 1 })
///    None
///
pub fn map(over option: Option(a), with fun: fn(a) -> b) -> Option(b) {
  case option {
    Some(x) -> Some(fun(x))
    None -> None
  }
}

/// Merge a nested Option into a single layer.
///
/// ## Examples
///
///    > flatten(Some(Some(1)))
///    Some(1)
///
///    > flatten(Some(None))
///    None
///
///    > flatten(None)
///    None
///
pub fn flatten(option: Option(Option(a))) -> Option(a) {
  case option {
    Some(x) -> x
    None -> None
  }
}

/// Update a value held within the Some of an Option by calling a given function
/// on it, where the given function also returns an Option. The two Options are
/// then merged together into one Option.
///
/// If the Option is a None rather than Some the function is not called and the
/// Option stays the same.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that return Options.
///
/// ## Examples
///
///    > then(Some(1), fn(x) { Some(x + 1) })
///    Some(2)
///
///    > then(Some(1), fn(x) { Some(tuple("a", x)) })
///    Some(tuple("a", 1))
///
///    > then(Some(1), fn(x) { None })
///    None
///
///    > then(None, fn(x) { Some(x + 1) })
///    None
///
pub fn then(option: Option(a), apply fun: fn(a) -> Option(b)) -> Option(b) {
  case option {
    Some(x) -> fun(x)
    None -> None
  }
}

/// Return the first value if it is Some, otherwise return the second value.
///
/// ## Examples
///
///    > or(Some(1), Some(2))
///    Some(1)
///
///    > or(Some(1), None)
///    Some(1)
///
///    > or(None, Some(2))
///    Some(2)
///
///    > or(None, None)
///    None
///
pub fn or(first: Option(a), second: Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second
  }
}

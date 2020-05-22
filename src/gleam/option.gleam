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

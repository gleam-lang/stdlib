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

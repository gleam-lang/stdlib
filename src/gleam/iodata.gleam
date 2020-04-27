/// Iodata is a type used for efficiently building strings.
///
/// When we append one string to another the strings must be copied to a
/// new location in memory so that they can sit together. This behaviour
/// enables efficient reading of the string but copying can be expensive,
/// especially if we want to join many strings together.
///
/// Iodata is different in that it can be joined together in constant time
/// using minimal memory, and then can be efficiently converted to a string
/// using the `to_string` function.
///
pub external type Iodata;

/// Prepend a String onto the start of some Iodata.
///
/// Runs in constant time.
///
pub external fn prepend(to: Iodata, prefix: String) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

/// Append a String onto the end of some Iodata.
///
/// Runs in constant time.
///
pub external fn append(to: Iodata, suffix: String) -> Iodata =
  "gleam_stdlib" "iodata_append";

/// Prepend some Iodata onto the start of another.
///
/// Runs in constant time.
///
pub external fn prepend_iodata(to: Iodata, prefix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

/// Append some Iodata onto the end of another.
///
/// Runs in constant time.
///
pub external fn append_iodata(to: Iodata, suffix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_append";

/// Convert a list of strings into iodata.
///
/// Runs in constant time.
///
pub external fn from_strings(List(String)) -> Iodata =
  "gleam_stdlib" "identity";

/// Joins a list of iodata into a single iodata.
///
/// Runs in constant time.
///
pub external fn concat(List(Iodata)) -> Iodata =
  "gleam_stdlib" "identity";

/// Convert a string into iodata.
///
/// Runs in constant time.
///
pub external fn new(String) -> Iodata =
  "gleam_stdlib" "identity";

/// Turns an `Iodata` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub external fn to_string(Iodata) -> String =
  "erlang" "iolist_to_binary";

/// Returns the size of the Iodata in bytes.
///
pub external fn byte_size(Iodata) -> Int =
  "erlang" "iolist_size";

/// Creates textual representation of the given float as iodata.
///
pub external fn from_float(Float) -> Iodata =
  "io_lib_format" "fwrite_g";

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// lowercased.
///
pub external fn lowercase(Iodata) -> Iodata = "string" "lowercase"

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// uppercased.
///
pub external fn uppercase(Iodata) -> Iodata = "string" "uppercase"

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// reversed.
///
pub external fn reverse(Iodata) -> Iodata = "string" "reverse"

type Direction {
  All
}

external fn erl_split(Iodata, String, Direction) -> List(Iodata) =
  "string" "split"

/// Splits iodata on a given pattern into a list of iodata.
///
pub fn split(iodata: Iodata, on pattern: String) -> List(Iodata) {
  erl_split(iodata, pattern, All)
}

external fn erl_replace(Iodata, String, String, Direction) -> Iodata =
  "string" "replace"

/// Replaces all instances of a pattern with a given string substitute.
///
pub fn replace(
  in iodata: Iodata,
  each pattern: String,
  with substitute: String,
) -> Iodata {
  erl_replace(iodata, pattern, substitute, All)
}

/// Compare two pieces of iodata to determine if they have the same textual
/// content.
///
/// Comparing two iodata using the `==` operator may return False even if they
/// have the same content as they may have been build in different ways, so
/// using this function is often preferred.
///
/// ## Examples
///
///    > from_strings(["a", "b"]) == new("ab")
///    False
///
///    > is_equal(from_strings(["a", "b"]), new("ab"))
///    True
///
///
pub external fn is_equal(Iodata, Iodata) -> Bool = "string" "equal"

/// Inspect some iodata to determine if it is equivalent to an empty string.
///
/// ## Examples
///
///    > new("ok") |> is_empty
///    False
///
///    > new("") |> is_empty
///    True
///
///    > from_strings([]) |> is_empty
///    True
///
///
pub external fn is_empty(Iodata) -> Bool = "string" "is_empty"

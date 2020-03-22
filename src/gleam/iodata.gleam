/// Represents Erlang's `iodata` type and the associated functions
/// to work with it.

pub external type Iodata;

/// Prepends the string `prefix` to `to`
///
pub external fn prepend(to: Iodata, prefix: String) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

/// Appends the string `suffix` to `to`
///
pub external fn append(to: Iodata, suffix: String) -> Iodata =
  "gleam_stdlib" "iodata_append";

/// Prepends the Iodata `prefix` to `to`
///
pub external fn prepend_iodata(to: Iodata, prefix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_prepend";

/// Prepends the Iodata `prefix` to `to`
///
pub external fn append_iodata(to: Iodata, suffix: Iodata) -> Iodata =
  "gleam_stdlib" "iodata_append";

/// Builds an Iodata value from the supplied list of strings
///
pub external fn from_strings(List(String)) -> Iodata =
  "gleam_stdlib" "identity";

/// Builds a new Iodata out of a list of Iodata
///
pub external fn concat(List(Iodata)) -> Iodata =
  "gleam_stdlib" "identity";

/// Turns a `String` into an `Iodata`
///
pub external fn new(String) -> Iodata =
  "gleam_stdlib" "identity";

/// Turns an `Iodata` into a `String`
///
pub external fn to_string(Iodata) -> String =
  "erlang" "iolist_to_binary";

/// Returns the size of the Iodata in bytes
///
pub external fn byte_size(Iodata) -> Int =
  "erlang" "iolist_size";

/// Creates an `Iodata` value from a `Float` value.
///
pub external fn from_float(Float) -> Iodata =
  "io_lib_format" "fwrite_g";

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// lowercased 
///
pub external fn lowercase(Iodata) -> Iodata = "string" "lowercase"

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// uppercased 
///
pub external fn uppercase(Iodata) -> Iodata = "string" "uppercase"

/// Converts Iodata to a new Iodata where valid UTF-8 string data is
/// reversed
///
pub external fn reverse(Iodata) -> Iodata = "string" "reverse"

type Direction {
  All
}

external fn erl_split(Iodata, String, Direction) -> List(Iodata) =
  "string" "split"

/// Splits the supplied Iodata by the pattern `on`
///
pub fn split(iodata: Iodata, on pattern: String) -> List(Iodata) {
  erl_split(iodata, pattern, All)
}

external fn erl_replace(Iodata, String, String, Direction) -> Iodata =
  "string" "replace"

/// Replaces all instances of `all` with the string `with`
///
pub fn replace(
  in iodata: Iodata,
  each pattern: String,
  with substitute: String,
) -> Iodata {
  erl_replace(iodata, pattern, substitute, All)
}

/// Prepends the string `prefix` to `to`
///
pub external fn is_equal(Iodata, Iodata) -> Bool = "string" "equal"

/// Prepends the string `prefix` to `to`
///
pub external fn is_empty(Iodata) -> Bool = "string" "is_empty"

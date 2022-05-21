import gleam/string

/// Fetches the unix timestamp from the runtime
///
pub fn unix_timestamp() -> Int {
  do_unix_timestamp()
}

if javascript {
  external fn do_unix_timestamp() -> Int =
    "../gleam_stdlib.mjs" "unix_timestamp"
}

if erlang {
  external fn do_unix_timestamp() -> Int =
    "gleam_stdlib" "unix_timestamp"
}

/// Fetches the locale from the runtime.
///
/// Returns a Tuple of normalized Strings containing:
/// 1. ISO-639 language code in lowercase
/// 2. ISO-3166 country code in uppercase
///
pub fn get_locale() -> #(String, String) {
  let runtime_locale = do_get_locale()
  let language_code =
    string.slice(from: runtime_locale, at_index: 0, length: 2)
    |> string.lowercase
  let country_code =
    string.slice(from: runtime_locale, at_index: 0, length: 2)
    |> string.uppercase
  #(language_code, country_code)
}

if javascript {
  external fn do_get_locale() -> String =
    "../gleam_stdlib.mjs" "get_locale"
}

if erlang {
  external fn do_get_locale() -> String =
    "gleam_stdlib" "get_locale"
}

//// Function to interact with the host operating system.

import gleam/list
import gleam/string

// Internal type for erlang interop.
external type CharList

external fn os_getenv() -> List(CharList) =
  "os" "getenv"

external fn os_putenv(key: CharList, value: CharList) -> Bool =
  "os" "putenv"

external fn os_unsetenv(key: CharList) -> Bool =
  "os" "unsetenv"

external fn char_list_to_string(CharList) -> String =
  "erlang" "list_to_binary"

external fn string_to_char_list(String) -> CharList =
  "erlang" "binary_to_list"

/// Return all environment variables set on the system.
pub fn get_env() -> List(tuple(String, String)) {
  list.map(
    os_getenv(),
    fn(char_list) {
      assert Ok(value) = string.split_once(char_list_to_string(char_list), "=")
      value
    },
  )
}

/// Set an environment variable.
pub fn insert_env(key: String, value: String) -> Nil {
  let True = os_putenv(string_to_char_list(key), string_to_char_list(value))
  Nil
}

/// Delete an environment variable.
pub fn delete_env(key: String) -> Nil {
  let True = os_unsetenv(string_to_char_list(key))
  Nil
}

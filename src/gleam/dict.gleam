import gleam/option.{type Option}

/// A dictionary of keys and values.
///
/// Any type can be used for the keys and values of a dict, but all the keys
/// must be of the same type and all the values must be of the same type.
///
/// Each key can only be present in a dict once.
///
/// Dicts are not ordered in any way, and any unintentional ordering is not to
/// be relied upon in your code as it may change in future versions of Erlang
/// or Gleam.
///
/// See [the Erlang map module](https://erlang.org/doc/man/maps.html) for more
/// information.
///
pub type Dict(key, value)

/// Determines the number of key-value pairs in the dict.
/// This function runs in constant time and does not need to iterate the dict.
///
/// ## Examples
///
/// ```gleam
/// new() |> size
/// // -> 0
/// ```
///
/// ```gleam
/// new() |> insert("key", "value") |> size
/// // -> 1
/// ```
///
@external(erlang, "maps", "size")
@external(javascript, "../gleam_stdlib.mjs", "map_size")
pub fn size(dict: Dict(k, v)) -> Int

/// Determines whether or not the dict is empty.
///
/// ## Examples
///
/// ```gleam
/// new() |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// new() |> insert("b", 1) |> is_empty
/// // -> False
/// ```
///
pub fn is_empty(dict: Dict(k, v)) -> Bool {
  size(dict) == 0
}

/// Converts the dict to a list of 2-element tuples `#(key, value)`, one for
/// each key-value pair in the dict.
///
/// The tuples in the list have no specific order.
///
/// ## Examples
///
/// Calling `to_list` on an empty `dict` returns an empty list.
///
/// ```gleam
/// new() |> to_list
/// // -> []
/// ```
///
/// The ordering of elements in the resulting list is an implementation detail
/// that should not be relied upon.
///
/// ```gleam
/// new() |> insert("b", 1) |> insert("a", 0) |> insert("c", 2) |> to_list
/// // -> [#("a", 0), #("b", 1), #("c", 2)]
/// ```
///
@external(erlang, "maps", "to_list")
@external(javascript, "../gleam_stdlib.mjs", "map_to_list")
pub fn to_list(dict: Dict(k, v)) -> List(#(k, v))

/// Converts a list of 2-element tuples `#(key, value)` to a dict.
///
/// If two tuples have the same key the last one in the list will be the one
/// that is present in the dict.
///
@external(erlang, "maps", "from_list")
pub fn from_list(list: List(#(k, v))) -> Dict(k, v) {
  from_list_loop(list, new())
}

fn from_list_loop(
  over list: List(#(k, v)),
  from initial: Dict(k, v),
) -> Dict(k, v) {
  case list {
    [] -> initial
    [#(key, value), ..rest] -> from_list_loop(rest, insert(initial, key, value))
  }
}

/// Determines whether or not a value present in the dict for a given key.
///
/// ## Examples
///
/// ```gleam
/// new() |> insert("a", 0) |> has_key("a")
/// // -> True
/// ```
///
/// ```gleam
/// new() |> insert("a", 0) |> has_key("b")
/// // -> False
/// ```
///
pub fn has_key(dict: Dict(k, v), key: k) -> Bool {
  do_has_key(key, dict)
}

@external(erlang, "maps", "is_key")
fn do_has_key(key: k, dict: Dict(k, v)) -> Bool {
  get(dict, key) != Error(Nil)
}

/// Creates a fresh dict that contains no values.
///
@external(erlang, "maps", "new")
@external(javascript, "../gleam_stdlib.mjs", "new_map")
pub fn new() -> Dict(k, v)

/// Fetches a value from a dict for a given key.
///
/// The dict may not have a value for the key, so the value is wrapped in a
/// `Result`.
///
/// ## Examples
///
/// ```gleam
/// new() |> insert("a", 0) |> get("a")
/// // -> Ok(0)
/// ```
///
/// ```gleam
/// new() |> insert("a", 0) |> get("b")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "gleam_stdlib", "map_get")
@external(javascript, "../gleam_stdlib.mjs", "map_get")
pub fn get(from: Dict(k, v), get: k) -> Result(v, Nil)

/// Inserts a value into the dict with the given key.
///
/// If the dict already has a value for the given key then the value is
/// replaced with the new value.
///
/// ## Examples
///
/// ```gleam
/// new() |> insert("a", 0)
/// // -> from_list([#("a", 0)])
/// ```
///
/// ```gleam
/// new() |> insert("a", 0) |> insert("a", 5)
/// // -> from_list([#("a", 5)])
/// ```
///
pub fn insert(into dict: Dict(k, v), for key: k, insert value: v) -> Dict(k, v) {
  do_insert(key, value, dict)
}

@external(erlang, "maps", "put")
@external(javascript, "../gleam_stdlib.mjs", "map_insert")
fn do_insert(key: k, value: v, dict: Dict(k, v)) -> Dict(k, v)

/// Updates all values in a given dict by calling a given function on each key
/// and value.
///
/// ## Examples
///
/// ```gleam
/// from_list([#(3, 3), #(2, 4)])
/// |> map_values(fn(key, value) { key * value })
/// // -> from_list([#(3, 9), #(2, 8)])
/// ```
///
pub fn map_values(in dict: Dict(k, v), with fun: fn(k, v) -> a) -> Dict(k, a) {
  do_map_values(fun, dict)
}

@external(erlang, "maps", "map")
fn do_map_values(f: fn(k, v) -> a, dict: Dict(k, v)) -> Dict(k, a) {
  let f = fn(dict, k, v) { insert(dict, k, f(k, v)) }
  fold(dict, from: new(), with: f)
}

/// Gets a list of all keys in a given dict.
///
/// Dicts are not ordered so the keys are not returned in any specific order. Do
/// not write code that relies on the order keys are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> keys
/// // -> ["a", "b"]
/// ```
///
@external(erlang, "maps", "keys")
pub fn keys(dict: Dict(k, v)) -> List(k) {
  do_keys_loop(to_list(dict), [])
}

fn do_keys_loop(list: List(#(k, v)), acc: List(k)) -> List(k) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [#(key, _value), ..rest] -> do_keys_loop(rest, [key, ..acc])
  }
}

fn reverse_and_concat(remaining: List(a), accumulator: List(a)) -> List(a) {
  case remaining {
    [] -> accumulator
    [first, ..rest] -> reverse_and_concat(rest, [first, ..accumulator])
  }
}

/// Gets a list of all values in a given dict.
///
/// Dicts are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order values are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> values
/// // -> [0, 1]
/// ```
///
@external(erlang, "maps", "values")
pub fn values(dict: Dict(k, v)) -> List(v) {
  let list_of_pairs = to_list(dict)
  do_values_loop(list_of_pairs, [])
}

fn do_values_loop(list: List(#(k, v)), acc: List(v)) -> List(v) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [#(_key, value), ..rest] -> do_values_loop(rest, [value, ..acc])
  }
}

/// Creates a new dict from a given dict, minus any entries that a given function
/// returns `False` for.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)])
/// |> filter(fn(key, value) { value != 0 })
/// // -> from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)])
/// |> filter(fn(key, value) { True })
/// // -> from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn filter(
  in dict: Dict(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> Dict(k, v) {
  do_filter(predicate, dict)
}

@external(erlang, "maps", "filter")
fn do_filter(f: fn(k, v) -> Bool, dict: Dict(k, v)) -> Dict(k, v) {
  let insert = fn(dict, k, v) {
    case f(k, v) {
      True -> insert(dict, k, v)
      False -> dict
    }
  }

  fold(dict, from: new(), with: insert)
}

/// Creates a new dict from a given dict, only including any entries for which the
/// keys are in a given list.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)])
/// |> take(["b"])
/// // -> from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)])
/// |> take(["a", "b", "c"])
/// // -> from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn take(from dict: Dict(k, v), keeping desired_keys: List(k)) -> Dict(k, v) {
  do_take(desired_keys, dict)
}

@external(erlang, "maps", "with")
fn do_take(desired_keys: List(k), dict: Dict(k, v)) -> Dict(k, v) {
  do_take_loop(dict, desired_keys, new())
}

fn do_take_loop(
  dict: Dict(k, v),
  desired_keys: List(k),
  acc: Dict(k, v),
) -> Dict(k, v) {
  let insert = fn(taken, key) {
    case get(dict, key) {
      Ok(value) -> insert(taken, key, value)
      Error(_) -> taken
    }
  }
  case desired_keys {
    [] -> acc
    [first, ..rest] -> do_take_loop(dict, rest, insert(acc, first))
  }
}

/// Creates a new dict from a pair of given dicts by combining their entries.
///
/// If there are entries with the same keys in both dicts the entry from the
/// second dict takes precedence.
///
/// ## Examples
///
/// ```gleam
/// let a = from_list([#("a", 0), #("b", 1)])
/// let b = from_list([#("b", 2), #("c", 3)])
/// merge(a, b)
/// // -> from_list([#("a", 0), #("b", 2), #("c", 3)])
/// ```
///
@external(erlang, "maps", "merge")
pub fn merge(into dict: Dict(k, v), from new_entries: Dict(k, v)) -> Dict(k, v) {
  new_entries
  |> to_list
  |> fold_inserts(dict)
}

fn fold_inserts(new_entries: List(#(k, v)), dict: Dict(k, v)) -> Dict(k, v) {
  case new_entries {
    [] -> dict
    [first, ..rest] -> fold_inserts(rest, insert_pair(dict, first))
  }
}

fn insert_pair(dict: Dict(k, v), pair: #(k, v)) -> Dict(k, v) {
  insert(dict, pair.0, pair.1)
}

/// Creates a new dict from a given dict with all the same entries except for the
/// one with a given key, if it exists.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> delete("a")
/// // -> from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> delete("c")
/// // -> from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn delete(from dict: Dict(k, v), delete key: k) -> Dict(k, v) {
  do_delete(key, dict)
}

@external(erlang, "maps", "remove")
@external(javascript, "../gleam_stdlib.mjs", "map_remove")
fn do_delete(a: k, b: Dict(k, v)) -> Dict(k, v)

/// Creates a new dict from a given dict with all the same entries except any with
/// keys found in a given list.
///
/// ## Examples
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> drop(["a"])
/// // -> from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> drop(["c"])
/// // -> from_list([#("a", 0), #("b", 1)])
/// ```
///
/// ```gleam
/// from_list([#("a", 0), #("b", 1)]) |> drop(["a", "b", "c"])
/// // -> from_list([])
/// ```
///
pub fn drop(from dict: Dict(k, v), drop disallowed_keys: List(k)) -> Dict(k, v) {
  case disallowed_keys {
    [] -> dict
    [first, ..rest] -> drop(delete(dict, first), rest)
  }
}

/// Creates a new dict with one entry inserted or updated using a given function.
///
/// If there was not an entry in the dict for the given key then the function
/// gets `None` as its argument, otherwise it gets `Some(value)`.
///
/// ## Example
///
/// ```gleam
/// let dict = from_list([#("a", 0)])
/// let increment = fn(x) {
///   case x {
///     Some(i) -> i + 1
///     None -> 0
///   }
/// }
///
/// upsert(dict, "a", increment)
/// // -> from_list([#("a", 1)])
///
/// upsert(dict, "b", increment)
/// // -> from_list([#("a", 0), #("b", 0)])
/// ```
///
pub fn upsert(
  in dict: Dict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> Dict(k, v) {
  case get(dict, key) {
    Ok(value) -> insert(dict, key, fun(option.Some(value)))
    Error(_) -> insert(dict, key, fun(option.None))
  }
}

/// Combines all entries into a single value by calling a given function on each
/// one.
///
/// Dicts are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order entries are used by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// # Examples
///
/// ```gleam
/// let dict = from_list([#("a", 1), #("b", 3), #("c", 9)])
/// fold(dict, 0, fn(accumulator, key, value) { accumulator + value })
/// // -> 13
/// ```
///
/// ```gleam
/// import gleam/string
///
/// let dict = from_list([#("a", 1), #("b", 3), #("c", 9)])
/// fold(dict, "", fn(accumulator, key, value) {
///   string.append(accumulator, key)
/// })
/// // -> "abc"
/// ```
///
pub fn fold(
  over dict: Dict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  fold_loop(to_list(dict), initial, fun)
}

fn fold_loop(
  list: List(#(k, v)),
  initial: acc,
  fun: fn(acc, k, v) -> acc,
) -> acc {
  case list {
    [] -> initial
    [#(k, v), ..rest] -> fold_loop(rest, fun(initial, k, v), fun)
  }
}

/// Calls a function for each key and value in a dict, discarding the return
/// value.
///
/// Useful for producing a side effect for every item of a dict.
///
/// ```gleam
/// import gleam/io
///
/// let dict = from_list([#("a", "apple"), #("b", "banana"), #("c", "cherry")])
///
/// each(dict, fn(k, v) {
///   io.println(key <> " => " <> value)
/// })
/// // -> Nil
/// // a => apple
/// // b => banana
/// // c => cherry
/// ```
///
/// The order of elements in the iteration is an implementation detail that
/// should not be relied upon.
///
pub fn each(dict: Dict(k, v), fun: fn(k, v) -> a) -> Nil {
  fold(dict, Nil, fn(nil, k, v) {
    fun(k, v)
    nil
  })
}

/// Creates a new dict from a pair of given dicts by combining their entries.
///
/// If there are entries with the same keys in both dicts the given function is
/// used to determine the new value to use in the resulting dict.
///
/// ## Examples
///
/// ```gleam
/// let a = from_list([#("a", 0), #("b", 1)])
/// let b = from_list([#("a", 2), #("c", 3)])
/// combine(a, b, fn(one, other) { one + other })
/// // -> from_list([#("a", 2), #("b", 1), #("c", 3)])
/// ```
///
pub fn combine(
  dict: Dict(k, v),
  other: Dict(k, v),
  with fun: fn(v, v) -> v,
) -> Dict(k, v) {
  use acc, key, value <- fold(over: dict, from: other)
  case get(acc, key) {
    Ok(other_value) -> insert(acc, key, fun(value, other_value))
    Error(_) -> insert(acc, key, value)
  }
}

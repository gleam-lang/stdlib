import gleam/result
import gleam/list

/// A dictionary of keys and values.
///
/// Any type can be used for the keys and values of a map, but all the keys
/// must be of the same type and all the values must be of the same type.
///
/// Each key can only be present in a map once.
///
/// Maps are not ordered in any way, and any unintentional ordering is not to
/// be relied upon in your code as it may change in future versions of Erlang
/// or Gleam.
///
/// See [the Erlang map module](https://erlang.org/doc/man/maps.html) for more
/// information.
///
pub external type Map(key, value)

/// Determine the number of key-value pairs in the map.
/// This function runs in constant time and does not need to iterate the map.
///
/// ## Examples
///
///    > new() |> size()
///    0
///
///    > new() |> insert("key", "value") |> size()
///    1
///
///
pub external fn size(Map(k, v)) -> Int =
  "maps" "size"

/// Convert the map to a list of 2-element tuples `tuple(key, value)`, one for
/// each key-value pair in the map.
///
/// The tuples in the list have no specific order.
///
/// ## Examples
///
///    > new() |> to_list()
///    []
///
///    > new() |> insert("key", 0) |> to_list()
///    [tuple("key", 0)]
///
pub external fn to_list(Map(key, value)) -> List(tuple(key, value)) =
  "maps" "to_list"

/// Convert a list of 2-element tuples `tuple(key, value)` to a map.
///
/// If two tuples have the same key the last one in the list will be the one
/// that is present in the map.
///
pub external fn from_list(List(tuple(key, value))) -> Map(key, value) =
  "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool =
  "maps" "is_key"

/// Determind whether or not a value present in the map for a given key.
///
/// ## Examples
///
///    > new() |> insert("a", 0) |> has_key("a")
///    True
///
///    > new() |> insert("a", 0) |> has_key("b")
///    False
///
pub fn has_key(map: Map(k, v), key: k) -> Bool {
  is_key(key, map)
}

/// Create a fresh map that contains no values.
///
pub external fn new() -> Map(key, value) =
  "maps" "new"

/// Fetch a value from a map for a given key.
///
/// The map may not have a value for the key, so the value is wrapped in a
/// Result.
///
/// ## Examples
///
///    > new() |> insert("a", 0) |> get("a")
///    Ok(0)
///
///    > new() |> insert("a", 0) |> get("b")
///    Error(Nil)
///
pub external fn get(from: Map(key, value), get: key) -> Result(value, Nil) =
  "gleam_stdlib" "map_get"

external fn erl_insert(key, value, Map(key, value)) -> Map(key, value) =
  "maps" "put"

/// Insert a value into the map with the given key.
///
/// If the map already has a value for the given key then the value is
/// replaced with the new value.
///
/// ## Examples
///
///    > new() |> insert("a", 0) |> to_list
///    [tuple("a", 0)]
///
///    > new() |> insert("a", 0) |> insert("a", 5) |> to_list
///    [tuple("a", 5)]
///
pub fn insert(into map: Map(k, v), for key: k, insert value: v) -> Map(k, v) {
  erl_insert(key, value, map)
}

external fn erl_map_values(fn(key, a) -> b, Map(key, value)) -> Map(key, b) =
  "maps" "map"

/// Update all values in a given map by calling a given function on each key
/// and value.
///
/// ## Examples
///
///    > [tuple(3, 3), tuple(2, 4)]
///    > |> from_list
///    > |> map_values(fn(key, value) { key * value })
///    [tuple(3, 9), tuple(2, 8)]
///
///
pub fn map_values(in map: Map(k, v), with fun: fn(k, v) -> w) -> Map(k, w) {
  erl_map_values(fun, map)
}

/// Get a list of all keys in a given map.
///
/// Maps are not ordered so the keys are not returned in any specific order. Do
/// not write code that relies on the order keys are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
///    > keys([tuple("a", 0), tuple("b", 1)])
///    ["a", "b"]
///
pub external fn keys(Map(keys, v)) -> List(keys) =
  "maps" "keys"

/// Get a list of all values in a given map.
///
/// Maps are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order values are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
///    > keys(from_list([tuple("a", 0), tuple("b", 1)]))
///    [0, 1]
///
pub external fn values(Map(k, values)) -> List(values) =
  "maps" "values"

external fn erl_filter(
  fn(key, value) -> Bool,
  Map(key, value),
) -> Map(key, value) =
  "maps" "filter"

/// Create a new map from a given map, minus any entries that a given function
/// returns False for.
///
/// ## Examples
///
///    > from_list([tuple("a", 0), tuple("b", 1)])
///    > |> filter(fn(key, value) { value != 0 })
///    from_list([tuple("b", 1)])
///
///    > from_list([tuple("a", 0), tuple("b", 1)])
///    > |> filter(fn(key, value) { True })
///    from_list([tuple("a", 0), tuple("b", 1)])
///
pub fn filter(in map: Map(k, v), for property: fn(k, v) -> Bool) -> Map(k, v) {
  erl_filter(property, map)
}

external fn erl_take(List(k), Map(k, v)) -> Map(k, v) =
  "maps" "with"

/// Create a new map from a given map, only including any entries for which the
/// keys are in a given list.
///
/// ## Examples
///
///    > from_list([tuple("a", 0), tuple("b", 1)])
///    > |> take(["b"])
///    from_list([tuple("b", 1)])
///
///    > from_list([tuple("a", 0), tuple("b", 1)])
///    > |> take(["a", "b", "c"])
///    from_list([tuple("a", 0), tuple("b", 1)])
///
pub fn take(from map: Map(k, v), keeping desired_keys: List(k)) -> Map(k, v) {
  erl_take(desired_keys, map)
}

/// Create a new map from a pair of given maps by combining their entries.
///
/// If there are entries with the same keys in both maps the entry from the
/// second map takes precedence.
///
/// ## Examples
///
///    > let a = from_list([tuple("a", 0), tuple("b", 1)])
///    > let b = from_list([tuple("b", 2), tuple("c", 3)])
///    > merge(a, b)
///    from_list([tuple("a", 0), tuple("b", 2), tuple("c", 3)])
///
pub external fn merge(into: Map(k, v), merge: Map(k, v)) -> Map(k, v) =
  "maps" "merge"

external fn erl_delete(k, Map(k, v)) -> Map(k, v) =
  "maps" "remove"

/// Create a new map from a given map with all the same entries except for the
/// one with a given key, if it exists.
///
/// ## Examples
///
///    > delete([tuple("a", 0), tuple("b", 1)], "a")
///    from_list([tuple("b", 1)])
///
///    > delete([tuple("a", 0), tuple("b", 1)], "c")
///    from_list([tuple("a", 0), tuple("b", 1)])
///
pub fn delete(from map: Map(k, v), delete key: k) -> Map(k, v) {
  erl_delete(key, map)
}

/// Create a new map from a given map with all the same entries except any with
/// keys found in a given list.
///
/// ## Examples
///
///    > drop([tuple("a", 0), tuple("b", 1)], ["a"])
///    from_list([tuple("b", 2)])
///
///    > delete([tuple("a", 0), tuple("b", 1)], ["c"])
///    from_list([tuple("a", 0), tuple("b", 1)])
///
///    > drop([tuple("a", 0), tuple("b", 1)], ["a", "b", "c"])
///    from_list([])
///
pub fn drop(from map: Map(k, v), drop disallowed_keys: List(k)) -> Map(k, v) {
  list.fold(disallowed_keys, map, fn(key, acc) { delete(acc, key) })
}

/// Create a new map with one entry updated using a given function.
///
/// If there was not an entry in the map for the given key then the function
/// gets `Error(Nil)` as its argument, otherwise it gets `Ok(value)`.
///
/// ## Example
///
///    > let increment = fn(x) {
///    >   case x {
///    >     Ok(i) -> i + 1
///    >     Error(Nil) -> 0
///    >   }
///    > }
///    > let map = from_list([tuple("a", 0)])
///    >
///    > update(map, "a" increment)
///    from_list([tuple("a", 1)])
///
///    > update(map, "b" increment)
///    from_list([tuple("a", 0), tuple("b", 0)])
///
pub fn update(
  in map: Map(k, v),
  update key: k,
  with fun: fn(Result(v, Nil)) -> v,
) -> Map(k, v) {
  map
  |> get(key)
  |> fun
  |> insert(map, key, _)
}

fn do_fold(
  list: List(tuple(k, v)),
  initial: acc,
  fun: fn(k, v, acc) -> acc,
) -> acc {
  case list {
    [] -> initial
    [tuple(k, v), ..tail] -> do_fold(tail, fun(k, v, initial), fun)
  }
}

/// Combine all entries into a single value by calling a given function on each
/// one.
///
/// Maps are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order entries are used by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// # Examples
///
///    > let map = from_list([tuple("a", 1), tuple("b", 3), tuple("c", 9)])
///    > fold(map, 0, fn(key, value, accumulator) { accumulator + value })
///    13
///
///    > import gleam/string.{append}
///    > fold(map, "", fn(key, value, accumulator) { append(accumulator, value) })
///    "abc"
///
pub fn fold(
  over map: Map(k, v),
  from initial: acc,
  with fun: fn(k, v, acc) -> acc,
) -> acc {
  map
  |> to_list
  |> do_fold(initial, fun)
}

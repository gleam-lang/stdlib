import gleam/result
import gleam/list
import gleam/result.{Option}

/// A dictionary of keys and values.
///
/// Any type can be used for the keys and values of a map, but all the keys
/// must be of the same type and all the values must be of the same type.
///
/// Each key can only be present in a map once.
///
/// Maps are not ordered in any way, and any unintentional ordering is not to
/// be relied upon in your code.
///
/// See [the Erlang map module](https://erlang.org/doc/man/maps.html) for more
/// information.
///
pub external type Map(key, value);

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
pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

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
///
pub external fn to_list(Map(key, value)) -> List(tuple(key, value))
  = "maps" "to_list"

/// Convert a list of 2-element tuples `tuple(key, value)` to a map.
///
/// If two tuples have the same key the last one in the list will be the one
/// that is present in the map.
///
pub external fn from_list(List(tuple(key, value))) -> Map(key, value)
  = "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool
  = "maps" "is_key"

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
///
pub fn has_key(map: Map(k, v), key: k) -> Bool {
  is_key(key, map)
}


/// Create a new map that contains no values.
///
pub external fn new() -> Map(key, value)
  = "maps" "new"

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
///
pub external fn get(from: Map(key, value), get: key) -> Option(value)
  = "gleam_stdlib" "map_get";

external fn erl_insert(key, value, Map(key, value)) -> Map(key, value)
  = "maps" "put";

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
///
pub fn insert(into map: Map(k, v), for key: k, insert value: v) -> Map(k, v) {
  erl_insert(key, value, map)
}

external fn erl_map_values(fn(key, a) -> b, Map(key, value))
  -> Map(key, b)
  = "maps" "map";

pub fn map_values(in map: Map(k, v), with fun: fn(k, v) -> w) -> Map(k, w) {
  erl_map_values(fun, map)
}

pub external fn keys(Map(keys, v)) -> List(keys)
  = "maps" "keys"

pub external fn values(Map(k, values)) -> List(values)
  = "maps" "values"

external fn erl_filter(fn(key, value) -> Bool, Map(key, value))
  -> Map(key, value)
  = "maps" "filter";

pub fn filter(in map: Map(k, v), for predicate: fn(k, v) -> Bool) -> Map(k, v) {
  erl_filter(predicate, map)
}

external fn erl_take(List(k), Map(k, v)) -> Map(k, v)
  = "maps" "with"

pub fn take(from map: Map(k, v), drop desired_keys: List(k)) -> Map(k, v) {
  erl_take(desired_keys, map)
}

pub external fn merge(into: Map(k, v), merge: Map(k, v)) -> Map(k, v)
  = "maps" "merge"

external fn erl_delete(k, Map(k, v)) -> Map(k, v)
  = "maps" "remove"

pub fn delete(from map: Map(k, v), delete key: k) -> Map(k, v) {
  erl_delete(key, map)
}

pub fn drop(from map: Map(k, v), drop disallowed_keys: List(k)) -> Map(k, v) {
  list.fold(disallowed_keys, map, fn(key, acc) {
    delete(acc, key)
  })
}

pub fn update(
  in map: Map(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> Map(k, v) {
  map |> get(_, key) |> fun |> insert(map, key, _)
}

fn do_fold(
  list: List(tuple(k, v)),
  initial: acc,
  fun: fn(k, v, acc) -> acc,
) -> acc {
  case list {
    [] -> initial
    [tuple(k, v) | tail] -> do_fold(tail, fun(k, v, initial), fun)
  }
}

pub fn fold(
  map: Map(k, v),
  from initial: acc,
  with fun: fn(k, v, acc) -> acc,
) -> acc {
  map |> to_list |> do_fold(_, initial, fun)
}

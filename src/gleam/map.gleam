import gleam/result
import gleam/list
import gleam/result.{Option}

/// An Erlang map. See [the Erlang map module](https://erlang.org/doc/man/maps.html) for details
pub external type Map(key, value);

pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

pub external fn to_list(Map(key, value)) -> List(tuple(key, value))
  = "maps" "to_list"

pub external fn from_list(List(tuple(key, value))) -> Map(key, value)
  = "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool
  = "maps" "is_key"

pub fn has_key(map: Map(k, v), key: k) -> Bool {
  is_key(key, map)
}

pub external fn new() -> Map(key, value)
  = "maps" "new"

pub external fn get(from: Map(key, value), get: key) -> Option(value)
  = "gleam_stdlib" "map_get";

external fn erl_insert(key, value, Map(key, value)) -> Map(key, value)
  = "maps" "put";

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
  map |> get(key) |> fun |> insert(map, key, _)
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
  map |> to_list |> do_fold(initial, fun)
}

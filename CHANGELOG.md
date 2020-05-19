# Changelog

## Unreleased

- The `string` module gains `trim`, `trim_left`, `trim_right`, `starts_with`,
  `ends_with`, `slice`, `pad_left` and `pad_right` functions.
- `uri` module created with `parse`, `parse_query`, `path_segments`,
  `query_to_string` and `to_string`.
- The `dynamic` module gains the `tuple2`, and `tuple2_of` functions.

## v0.8.0 - 2020-04-28

- The error type for `atom.from_string` has been renamed to `FromStringError`.
- The `string` module gains `contains` and `repeat` functions.
- The `expect` module has been renamed to `should`. Functions in the module
  starting with `is_` have been changed to `be_`.
- The `string.replace` and `iodata.replace` `all` arguement label has been
  changed to `each`.
- The `string` module gains `is_empty`, `join` and `concat` functions.
- The `int` module gains `is_even` and `is_odd` functions.
- The `list.length` function now accepts a labelled argument.
- The `list.length` function now accepts a labelled argument.
- The the second argument of `bool.compare`, `float.compare`, `int.compare`,
  and `order.compare` now have the label `with`.
- The `dynamic.unsafe_coerce` function now only accepts Dynamic data.
- The `dynamic` decoder functions no longer print the entire value in their
  error messages, to avoid large errors.

## v0.7.0 - 2020-03-03

- The `result` module gains an `Option` type alias.
- The `function` module has been created with `identity`, `compose`, and
  `flip` functions.
- The error type of `list.find_map` is now `Nil`.
- The labels for `list.split` are now `split(list: _, at: _)`.

## v0.6.0 - 2019-12-23

- Syntax has been updated for Gleam v0.6.0.
- The `dynamic` module gains an `element` for decoding tuples.

## v0.5.0 - 2019-12-16

- Syntax has been updated for Gleam v0.5.
- Labels have been added to functions throughout the stdlib.
- `map.fetch` has been renamed to `map.get` and `map.put` to `map.insert`.
- `list.find` has been renamed `list.find_map` and a new `list.find` has been
  introduced.
- The `pair` module gains the `map_first`, and `map_second` functions.
- The `pair.Pair` type has been replaced with a 2 element anonymous struct.
- The `triple` module has been removed.
- The `string` module gains the `compare` function.
- The `float` module gains the `max`,  and `min` functions.
- The `int` module gains the `max`, and `min` functions.
- The `Any` type and module have been renamed to `Dynamic`.

## v0.4.0 - 2019-09-19

- Syntax has been updated for Gleam v0.4.
- The `map_dict` module has been renamed to `map`.
- `list:sort` now requires a compare function as comparison operators
  now only work on Ints.
- `list:sort`'s performance has been slightly optimised.
- The `float` module gains a `compare` function.
- `any.tuple` has been renamed `any.pair`.
- The `tuple` module has been renamed to `pair` and has a `Pair` type.
- `pair.fetch` has been replaced with `list.key_find`.
- `triple` module has been created with type `Triple`.
- The error type for `float.parse`, `int.parse`, `list.head`, `list.tail`,
  `list.find`, `list.at`, `map.fetch`, and `map.update` is now `Nil`.

## v0.3.1 - 2019-08-08

- `result:map_error` has been relaxed to allow mapping to a different error
  type.

## v0.3.0 - 2019-06-25

- The `map_dict` module gains a `fold` function.
- All modules moved under the `std` namespace.
- The `http` module has been split out into the `gleam_http` package.

## v0.2.0 - 2019-05-11

- Library renamed to `gleam_stdlib`.
- The `map_dict` module gains `update`, `merge` and `delete` functions.
- The `bool` module gains a `compare` function.
- The `int` module gains a `compare` function.
- The `list` module gains `range`, `repeat`, `split`, `split_while` and
  `strict_zip` functions.

## v0.1.2 - 2019-04-25

- The `list` module gains `at`, `all`, `any`, `index_map`, `intersperse`,
  `sort`, `unique`, and `zip` functions.
- `map_dict:Map` renamed to `map_dict:MapDict`.
- The `map_dict` module gains `drop`, and `take` functions.
- The `str` module gains `append` function and loses `from_int`, `parse_int`,
  `from_float`, `parse_float`, and `base_from_int`.
- `int` module created with `parse`, `to_string`, and `to_base_string`.
- `float` module created with `ceiling`, `floor`, `round`, `truncate`,
  `parse`, and `to_string`.

## v0.1.1 - 2019-04-17

- Included missing gleam.toml in hex package.

## v0.1.0 - 2019-04-15

- Initial release!

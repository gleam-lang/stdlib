# Changelog

## Unreleased

- The performance of various functions in the `list` module has been improved.
- Fixed the implementation of `option.values` and `option.all` to be tail
  recursive.

## v0.59.0 - 2025-04-07

- The `debug` function in the `io` module has been deprecated in favour of
  the `echo` keyword.
- The performance of `string.append`, `string.join`, and `string.concat` have
  been improved.

## v0.58.0 - 2025-03-23

- The deprecated `pop` and `pop_map` functions have been removed from the
  `list` module.

## v0.57.0 - 2025-03-11

- The minimum supported Gleam version has been increased to 1.9.0.
- The functions in the `bit_array` module now support unaligned bit arrays on
  the JavaScript target.
- Fixed a bug where tuples with atoms in the first position could be formatted
  incorrectly by `string.inspect`.

## v0.56.0 - 2025-03-09

- The decode API can now index into the first 8 elements of lists.

## v0.55.0 - 2025-02-21

- The performance of `dict.is_empty` has been improved.
- The `flip` function in the `function` module has been deprecated.
- The `uri` module gains the `empty` value, representing an empty URI which
  equivalent to `""`.

## v0.54.0 - 2025-02-04

- The deprecated `drop_left`, `drop_right`, `pad_left`, `pad_right`,
  `trim_left`, and `trim_right` functions have been removed.
- Fixed a bug that would result in `list.unique` having quadratic runtime.
- Fixed the implementation of `list.key_set` to be tail recursive.
- The `pop` and `pop_map` functions in the `list` module have been deprecated.

## v0.53.0 - 2025-01-23

- `io.print` will now work in JavaScript environments where the `process`
  object exists but does not have a `stdout` property.
- The decoder API in the `dynamic` module has been deprecated.

## v0.52.0 - 2025-01-04

- Improved the precision of `float.to_precision`.
- The deprecated `function.compose`, `function.constant`, `function.apply*`,
  `function.curry*`, `result.nil_error`, `list.concat`, `bool.compare`, and
  `bool.to_int` functions have been removed.
- Fixed a bug where `string.utf_codepoint` would treat valid Unicode codepoints
  `U+FFFE` and `U+FFFF` as invalid.

## v0.51.0 - 2024-12-22

- `dynamic/decode` now has its own error type.
- The `new_primitive_decoder` function in the `dynamic/decode` has a new API.

## v0.50.0 - 2024-12-22

- The `dynamic/decode` module has been added. This module will replace the
  dynamic decoding combinators from `dynamic` (`decode3`, etc) in future.
- The deprecated `iterator`, `regex`, and `queue` modules have been removed.

## v0.49.0 - 2024-12-19

- The `float` module gains the `exponential` and `logarithm` function.
- The `list` module gains the `max` and `sample` function.

## v0.48.0 - 2024-12-17

- Fixed a bug where `string.utf_codepoint` would erronously accept negative input.
- The deprecated `string_builder` and `bytes_builder` modules have been removed.

## v0.47.0 - 2024-12-10

- The `compare` and `to_int` functions from the `gleam/bool` module have been
  deprecated.

## v0.46.0 - 2024-12-08

- Improved the performance of comparing two `Dict`s of equal size on the
  JavaScript target.
- Improved the performance of `string.drop_start`.
- Improved the performance of `list.strict_zip`.
- Fixed a bug where `string.trim` could fail on JavaScript.
- Fixed a bug where `string.trim` wouldn't trim multi-line string on JavaScript.

## v0.45.0 - 2024-11-28

- The performance of `string.trim`, `string.trim_start`, and `string.trim_end`
  has been improved on JavaScript.
- The `base64_encode`, `base64_url_encode`, and `base16_encode` functions in the
  `bit_array` module no longer throw an exception when called with a bit array
  which is not a whole number of bytes. Instead, the bit array is now padded
  with zero bits prior to being encoded.
- The `bit_array` module gains the `pad_to_bytes` function.
- The `bytes_tree` module now pads unaligned bit arrays with zeros when they are
  added to the tree.

## v0.44.0 - 2024-11-25

- The `gleam/queue` module has been deprecated in favour of the `gleam_deque`
  package.
- The `gleam/iterator` module has been deprecated in favour of the
  `gleam_yielder` package.
- The `gleam/regex` module has been deprecated in favour of the `gleam_regexp`
  package.

## v0.43.0 - 2024-11-17

- `BytesBuilder` is now an alias of `BytesTree`.
- `StringBuilder` is now an alias of `StringTree`.

## v0.42.0 - 2024-11-15

- The `bit_array` module gains the `bit_size` and `starts_with` functions.
- The `string` module gains the `drop_start`, `drop_end`, `pad_start`,
  `pad_end`, `trim_start`, and `trim_end` functions. These replace the
  `drop_left`, `drop_right`, `pad_left`, `pad_right`, `trim_left`, and
  `trim_right` functions, which have been deprecated.
- The `result.nil_error` function has been deprecated in favour of
  `result.replace_error`.
- The `gleam/bytes_builder` module has been deprecated in favour of the
  `gleam/bytes_tree` module.
- The `gleam/string_builder` module has been deprecated in favour of the
  `gleam/string_tree` module.

## v0.41.0 - 2024-10-31

Happy Samhain! 🎃

- The `bit_array` module gains the `compare` function.
- The `float` module gains the `to_precision` function.
- The `iterator.try_fold` function is now tail recursive.
- The performance of many functions in the `string` module has been improved.
- The `list.concat` function has been deprecated in favour of `list.flatten`.
- The handling of float exponentials and signs in the `float.to_string` and
  `string.inspect` functions have been improved on JavaScript.
- The `set` module gains the `each` function.

## v0.40.0 - 2024-08-19

- The `function.curry*` and `function.apply*` functions have been deprecated.
- The deprecated `dynamic.unsafe_coerce` function has been removed.
- The deprecated `dict.update` function has been removed.
- The deprecated `order.max` and `order.min` functions have been removed.
- The `float` module gains the `modulo` function.
- The `uri.origin` function no longer incorrectly has a trailing slash.
- The `dynamic.optional_field` decoder no longer treats the value as implicitly
  optional. It only deals with the presence or absence of the key itself which
  brings it inline with its documentation.
- Fixed a bug where `string.trim` could remove commas on JavaScript.
- The `string.pop_grapheme` function has been optimised on Erlang, greatly
  improving its performance.
- The `InvalidBase` error in the `int` module has been replaced by `Nil`.
- Fixed a bug where iterating graphemes could crash on older JavaScript runtimes
  where the `Intl` API is not supported.
- Fixed a bug where the behaviour of `uri.percent_decode` would decode `+` as a
  space on JavaScript.
- Fixed a bug where `string.slice` could return invalid values on Erlang.

## v0.39.0 - 2024-07-09

- Fixed `list.window` entering an endless recursive loop for `n` = 0.
- The `min` and `max` functions of the `order` module have been deprecated.
- The `dict` and `set` modules gain the `is_empty` function.
- The `set` module gains the `map` function.
- Fixed `string.inspect` not formatting ASCII escape codes on Erlang that could
  lead to unexpected behavior. Now, all ASCII escape codes less than 32, as well
  as escape code 127, are converted into `\u{xxxx}` syntax, except for common
  escape codes such as `\n` or `\r`.
- Fixed a bug where `string.inspect` would use the incorrect syntax for unicode
  escape codes on JavaScript.
- The `list` module gains the `count` function.
- The `update` function of the `dict` module has been deprecated in favour
  of `upsert` and `update` will be used with a different signature in future.
- The behaviour of the string trim functions is now consistent across targets.
- `iterator.yield` now yields values without waiting for the next one to become
  available.
- Improved bit array Base64 encoding and decoding speed on JavaScript.
- Fixed a bug where Base64 encoding a bit array larger than ~100KiB would throw
  an exception on JavaScript.
- Fixed `float.parse` failing to parse exponential notation on JavaScript.
- The `regex` module gains the `replace` function.

## v0.38.0 - 2024-05-24

- The `set` module gains the `is_subset`, `is_disjoint`, and `symmetric_difference` functions
- The `sort` function of the `list` module has been optimised in case it's
  working on an already sorted list.
- The `dict` module gains the `each` function.
- The `list` module gains the `wrap` function.
- The `iterator` module gains the `find_map` function.
- Fixed `string.inspect` not formatting the `\f` form feed control character
  correctly on Erlang.
- `dynamic.unsafe_coerce` function has been deprecated.
- Fixed `bit_array` slices of slices sometimes being incorrect on JavaScript.
- The `dict` module gains the `combine` function.
- The deprecated `list.at`, `bool.max`, and `bool.min` functions has been
  removed.

## v0.37.0 - 2024-04-19

- The `order` module gains the `break_tie` and `lazy_break_tie` functions.
- `list.at` has been deprecated as it was misleading and would be commonly
  misused, resulting in poor code.
- `list.LengthMismatch` has been removed.
- The mistakenly public `bit_array.do_inspect` function has been removed.
- Fixed the `dynamic.classification` function for bools.
- The `min` function in the `bool` module has been deprecated in favour of
  `bool.and`.
- The `max` function in the `bool` module has been deprecated in favour of
  `bool.or`.
- Fixed a bug with `regex.split` where it could include `Nil` elements in the
  returned list of strings on the JavaScript target when the expression to
  split with included an optional match group which wasn't matched.

## v0.36.0 - 2024-02-26

- Fixed a bug where on JavaScript the `dynamic.field` function could crash when
  given `null`.
- The `compose` and `constant` functions in the `function` module have been
  deprecated in favour of the `fn` literal syntax.

## v0.35.1 - 2024-02-15

- Fixed a warning on the JavaScript target.

## v0.35.0 - 2024-02-15

- The `bit_array` module gains the `inspect` function.
- The base 64 encoding and decoding functions in the `bit_array` module no
  longer insert newlines in the output on JavaScript, making them consistent
  with the Erlang target.
- The `set` module gains the `difference` function.
- The deprecated `bit_string`, `bit_builder`, `base`, and `map` modules have
  been removed.
- The deprecated `map`, and `bit_string` functions in the `dynamic` module have
  been removed.

## v0.34.0 - 2023-12-17

- The `int.random` function now takes a single argument.
- The `float.random` function no longer takes any arguments.
- Changed `list.index_map` callback signature to `fn(a, Int) -> b` from
  `fn(Int, a) -> b`, to be consistent with `list.index_fold`.
- Changed `iterator.index` to return `Iterator(#(a, Int))` instead of
  `Iterator(#(Int, a))`.

## v0.33.1 - 2023-12-02

- Fixed `string.to_graphemes` failing on JavaScript runtimes when the
  `Intl.Segmenter` class is not available.

## v0.33.0 - 2023-11-30

- The `bool` module gains the `lazy_guard` function.
- The `gleam/map` module has been deprecated in favour of the `gleam/dict`
  module.
- The `map` function in the `gleam/dynamic` module has been deprecated in favour
  of the `dict` function.

## v0.32.1 - 2023-11-11

- The printing of maps by `string.inspect` has been improved.

## v0.32.0 - 2023-11-01

- The `set.filter` label `for` was renamed to `keeping`.
- The `map.filter` label `for` was renamed to `keeping`.
- The `iterator.filter` label `for` was renamed to `keeping`.
- The `list.filter` label `for` was renamed to `keeping`.
- Updated for Gleam v0.32.0 syntax.
- The `base` module has been deprecated in favour of the new `bit_array`
  module.
- The `bit_string` module has been deprecated in favour of the new `bit_array`
  module.
- The `bit_builder` module has been deprecated in favour of the new
  `bytes_builder` module.
- The `bit_array` module also contains the `base16_encode` and `base16_decode`
  functions.
- Improved performance of `string.to_graphemes` on JavaScript.
- The `iterator` module gains the `map2` function.
- The `list` module gains the `key_filter` function.
- Fixed a bug on target JavaScript where `Map` equality would not be correctly
  checked for maps of different sizes.
- Fixed a bug where non-byte aligned bit arrays would be printed suboptimally by
  `string.inspect`.

## v0.31.0 - 2023-09-25

- `list.flatten` is no longer deprecated and is kept as a synonym of
  `list.concat`
- The `iterator` module gains the `concat` function.
- The `int` module gains the `bitwise_and`, `bitwise_or`,
  `bitwise_exclusive_or`, `bitwise_not`, `bitwise_shift_left`, and
  `bitwise_shift_right` functions.

## v0.30.2 - 2023-08-31

- Fixed a bug where `base.decode64` could crash on the Erlang target.

## v0.30.1 - 2023-08-06

- Updated to Gleam v0.30.0 syntax.
- Atoms are now shown using `atom.create_from_string` when passed to
  `string.inspect`.
- Fixed a bug where `string.inspect` would show atoms as Gleam custom types even
  when the format is invalid.
- The `iterator` module gains the `yield` function.

## v0.30.0 - 2023-07-16

- The `list` module gains the `list.map2` function.
- `reverse` has been renamed to `negate` in the `order` module.
- A new `reverse` function is added to the `order` module, which reverses an
  ordering function.
- `flatten` has been renamed to `concat` in the `list` module. The old name is
  still available as an alias and is deprecated.

## v0.29.2 - 2023-06-21

- improve `string.join` and `string.concat` performance on JavaScript target.
- The `result` module gains the `try_recover` function.
- The `string` module gains the `byte_size` function.

## v0.29.1 - 2023-06-01

- Fixed a bug on target JavaScript where `regex.check` would not correctly
  execute while using the same regular expression in consecutive calls.
- The `zip` function's second argument in the `list` module gains the `with`
  label.
- The `strict_zip` function's second argument in the `list` module gains the
  `with` label.
- The `pair` module gains the `new` function.

## v0.29.0 - 2023-05-23

- The `result` module gains the `partition` function.
- `dynamic.tupleN` functions now support lists.

## v0.28.2 - 2023-05-09

- Fixed a bug where `dynamic.map` would crash when passed the JavaScript values
  of `null` or `undefined`.
- Fixed a bug where `io.debug` would crash when called in a React Native
  environment.

## v0.28.1 - 2023-04-10

- The `iterator` module gains the `each` function.
- Fixed a bug in maps when running on JavaScript where value membership could be
  incorrectly stated in some cases.
- `result.then` is now an alias of `result.try`; both do the same.
- The `list` module gains the `try_each` function.
- The `dynamic` module gains the `optional_field` function.

## v0.28.0 - 2023-03-26

- `regex.scan` now behaves consistently across both targets when a capture group
  does not capture anything.
- The `Map` type was rewritten as a persistent immutable data structure. This
  results in drastically improved performance when constructing or updating
  maps, especially with large maps.
- The `all` and `any` functions in the `iterator` module are now tail recursive.

## v0.27.0 - 2023-02-26

- The `bool` module gains the `guard` function.
- Fixed a bug where `io.print`, `io.print_error`, and `io.print_debug` would use
  `console.log` and add `"\n"` to the output when running on Deno.
- Fixed a bug where `int.floor_divide` would return the wrong result in certain
  edge-cases.
- The `iterator` module gains the `length` function.

## v0.26.1 - 2023-02-02

- The `prepend` function in the `list` module gains the `this` label.
- The `list` module gains the `group` function.
- The `dynamic` module is able to decode simple JavaScript objects to maps.
  So, the behaviour of the `field` and `object` functions are consistent.
- For a given empty list as an argument, `int.product` now returns `1` instead
  of `0`, and `float.product` now returns `1.0` instead of `0.0`. This mimics
  the behavior of Elixir's `Enum.product/1`.

## v0.26.0 - 2023-01-12

- The `dynamic` module gains the `decode1` function.
- The `float` module gains the `loosely_equals` function.
- The `io` module gains `print_error` and `println_error` functions for
  printing to stderr.
- The `set` module gains the `drop` function.
- The `io.debug` function now prints to stderr instead of stdout when using
  the Erlang target or running in Node.js (but still uses `console.log`
  when running as JavaScript in a browser)
- The `iterator` module gains the `transform` function.
- The `list.at` function now returns `Error(Nil)` if given index is smaller than
  zero, instead of returning the first element.
- Fixed a bug where some string functions would incorrectly handle newlines when
  iterating over graphemes in older JavaScript environments that do not have the
  `Intl.Segmenter` class.
- The `string` module gains `to_utf_codepoints`, `from_utf_codepoints`, and
  `utf_codepoint_to_int` functions.
- Fixed `string.inspect`'s escaping of `"`, `\`, `\n`, `\r`, `\r\n`, and `\t`,
  which in turn fixes `io.debug`'s output of such strings.
- The `bit_string` function in the `dynamic` module now knows how to handle
  JavaScript `Uint8Array`s.

## v0.25.0 - 2022-11-19

- The `bool` module gains the `and` and `or` functions.
- The `float` module gains the `add`, `subtract` and `multiply` functions.
- The `int` module gains the `add`, `subtract` and `multiply` functions.
- Fixed a bug where `list.permutations` would not correctly permutate lists
  with non-unique item values.
- For `regexp.compile` unicode character properties are now used when
  resolving `\B`, `\b`, `\D`, `\d`, `\S`, `\s`, `\W`, and `\w` on target
  Erlang.
- `list.sort` is now tail recursive and will no longer exceed the stack size
  on large inputs on target JavaScript.
- `list.sort` is now a "stable" sort, meaning elements which are equal in
  regards to the given comparison function will keep their previous order.
- Added functions `function.apply1` through `function.apply3` which help
  working with functions in pipelines.
- Fixed a bug where `regex.scan` would not work correctly on utf8.
- The performance of `list.flatten` has been greatly improved.
- The `string_builder` module gains the `join` function.
- The `list` module gains the `shuffle` function.
- `string.split` will now return a list of graphemes if split on an empty
  string (`""`).

## v0.24.0 - 2022-10-15

- `string.slice` is now tail recursive and will no longer exceed the stack size
  on large inputs on target JavaScript.
- Added `int.remainder` and `int.modulo` functions which allow safe remainder
  and modulo operations the way common languages support them.
- Added `int.floor_divide` to complement the truncated `int.divide`.

## v0.23.0 - 2022-09-15

- Fixed `string.inspect` and `io.debug` crashing on improper Erlang lists
  (#333).

## v0.22.3 - 2022-08-09

- Removed a duplicate import.

## v0.22.2 - 2022-08-09

- The list dynamic decoding functions can now decode JavaScript arrays into
  Gleam lists.
- `list.range` is now tail recursive and will not blow the stack with large
  ranges when compiled to JavaScript.
- Fixed a bug where the `list` module's `contains`, `any`, and `all` could
  exhaust the stack when compiling to JavaScript.
- `list.range` and `iterator.range` return values are now inclusive of both
  start and end bounds.

## v0.22.1 - 2022-06-27

- Fixed a bug where `big_string.concat` could crash.
- The `bit_builder` module gains the `from_bit_strings` function.
- Changed `list.contains`, `list.any`, `list.all` so that Gleam can do tail call
  optimization which fixes stack size crashes on Firefox, Chrome and NodeJS
  (#322).

## v0.22.0 - 2022-06-15

- The `float` module gains the `divide` function.
- The `int` module gains the `divide`, `power`, and `square_root` functions.
- The `string` module gains the `first`, `last`, `capitalise` and `inspect`
  functions.
- Fixed a bug where `string_builder.reverse` would break utf8 strings on target
  JavaScript.
- Fixed a bug where `string.reverse` would break utf8 strings on target
  JavaScript.
- Fixed a bug where `string.slice` would break utf8 strings on target
  JavaScript.
- The `string_builder` module loses the `from_float` function. Use
  `float.to_string` instead.
- Fixed the `int.power` and `float.power` functions by properly handling error
  cases.
- The grapheme iterator used by `string.graphemes` is now locale independent on
  target JavaScript.
- Unified `io.debug` to yield Gleam syntax to standard output (stdout) not just
  on JavaScript but also Erlang.

## v0.21.0 - 2022-04-24

- Fixed a bug where record-based map keys would clash in JavaScript.
- The `eunit` Erlang headers are no longer required to compile this package.
- Fixed a bug where the `string.length` function would cause a JavaScript error
  for empty strings on older JavaScript runtimes.
- The `bool` module gains the `to_string` function.
- The `function` module gains the `tap` function.
- The `float` module gains the `random` function.
- The `int` module gains the `random` function.
- The JavaScript target implementation of the `string.replace` received a bug
  fix.
- The `list` module gains a `prepend` function (#284).

## v0.20.0 - 2022-02-22

- The `dynamic` module gains the `decode9` function.
- The `float` module gains the `loosely_compare` function.
- The `string_builder` module gains the `new` function.
- The `bit_builder` module gains the `new` function.
- The `result` module gains the `replace`, `unwrap_both` and `unwrap_error`
  functions.

## v0.19.3 - 2022-01-14

- Fixed a bug where `io.print` and `io.println` may print unicode characters
  incorrectly.
- Fixed a bug where the `dynamic.field` function would return an incorrect error
  value.

## v0.19.2 - 2022-01-09

- The `dynamic.dynamic` function is is no longer a thunk.

## v0.19.1 - 2022-01-09

- The `dynamic.dynamic` function now returns a result.
- The `dynamic.map` function is now curried and requires the decoders for keys
  and values to be supplied.
- The `dynamic.result`, `dynamic.optional`, `dynamic.field`, and `dynamic.list`
  functions are now partially applied.

## v0.19.0 - 2022-01-09

- The `dynamic` module gains the `dynamic` function.
- The shallow `dynamic.list` function has been renamed to
  `dynamic.shallow_list`.
- The shallow `result`, and `tuple*` functions have been removed from the
  `dynamic` module.
- The `dynamic.typed_list` function has been renamed to `dynamic.list`.
- The `dynamic.typed_result` function has been renamed to `dynamic.result`.
- The `dynamic.any` is now available on JavaScript.
- The `dynamic.typed_tuple*` functions have been renamed to `dynamic.tuple*`.
- The `dynamic.field` and `dynamic.element` functions now requires the type of
  the field to be specified.
- The `dynamic.DecodeError` now has a `path` field.
- The decoder functions of the `dynamic` module now return multiple errors.
- The `dynamic.any`, `dynamic.element` and `dynamic.tuple*` functions are now
  partially applied.
- The `dynamic` module gains the `decode2`, `decode3`, `decode4`, `decode5`,
  `decode6`, `decode7`, and `decode8` functions.
- The `int` module gains the `digits` and `undigits` functions.
- The `option` module gains the `lazy_or` and `lazy_unwrap` functions.

## v0.18.1 - 2021-12-19

- The `function` module gains the `constant` function.
- The internal `gleam_stdlib.js` module has been renamed to `gleam_stdlib.mjs`.

## v0.18.0 - 2021-11-23

## v0.18.0-rc1 - 2021-11-23

- Converted to use the Gleam build tool, not rebar3.
- The `iterator` module gains the `first` and `at` functions.
- The `list` module renames the `head` and `tail` functions to `first` and
  `rest`.
- The `list.at` function now behaves uniformly to `iterator.at`.
- `int.to_base_string` now returns a `Result(Int, InvalidBase)`.
- The `int` module gains the `to_base2`, `to_base8`, `to_base16` and `to_base36`
  functions.

## v0.17.1 - 2021-09-15

- `uri.parse` now returns a result.

## v0.17.0 - 2021-09-11

- All modules have been updated to work on JavaScript as well as Erlang.
- The `bit_string` module gains the `concat` function and has the `part`
  function renamed to `slice`.
- The `os` module has been removed in favour of target specific libraries.
- The `rescue` function has been removed from the `function` library in favour
  of target specific versions in Erlang and JavaScript specific libraries.
- The `map.update` function now uses `Option` rather than `Result`.
- The `iterator` module gains the `fold_until` and `try_fold` functions.
- The `bit_string` module loses the u32 functions in favour of bit string
  literals.
- The `dynamic` module loses the `atom` function and gains the `classify`
  function.
- The `dynamic.option` function has been renamed to `optional` and made more
  permissive to other null values.
- The `dynamic.result` function has been made more permissive to other result
  values.
- The `dynamic.thunk` function has been removed.
- The `dynamic.element` label `position` was renamed to `get`.
- The `dynamic.element` now accepts negative indexes.
- The `io.get_line` function has been moved to the `gleam_erlang` library.
- The `atom` module has been moved to the `gleam_erlang` library.
- Prelude types like `Result`, `List` etc. are no longer redefined in their
  stdlib modules.
- The `dynamic` module functions now return structured error values instead of a
  string error description.
- The `string` module gains the `to_option` function.
- Fixed a bug where `io.print` could crash when printing special characters.
- The `regex.Match` record no longer has the `byte_index` field any more.
- The `should` module has been moved to the `gleam_should_assertions` package.
- The `uri.percent_encode` function has a slightly different behaviour. For
  example spaces are encoded as `%20`, not as `+`.
- The order of the arguments of the the function accepted by the
  `list.map_fold`, `list.fold`, `list.fold_right`, `list.index_fold`,
  `list.try_fold`, `list.fold_until`, `list.reduce`, `list.scan`, `map.fold`,
  `set.fold`, `iterator.fold`, `iterator.scan`, `iterator.reduce`,
  `iterator.fold_until`, and `iterator.try_fold` have been flipped.

## v0.16.0 - 2021-06-17

- The `list` module gains the `interleave`, `flat_map` and `transpose`
  functions.
- The `option` module gains the `all` and `values` functions.
- The `os` module now uses unicode to encode/decode environment variables.
  This fixes an issue when non-latin characters are present in environment.
- The `result` module gains the `values` function.
- All modules now use the new `#(a, b, ...)` tuple syntax.

## v0.15.0 - 2021-05-05

- The `list.split_while` function's second argument now has the label
  `satisfying` to match the other `_while` functions in `list` and `iterator`.
- The `dynamic` module gains the `tuple3`, `tuple4`, `tuple5`, `tuple6`
  functions and their typed equivalents `typed_tuple3`, `typed_tuple4`,
  `typed_tuple5`, `typed_tuple6`.
- The `list` module gains the `combinations`, `combination_pairs`, `drop_while`,
  `map_fold`, `take_while`, `reduce`, `chunk`, `sized_chunk`, `last` and `scan`
  functions.
- The `iterator` module gains the `index`, `iterate`, `zip`, `scan`, `last`,
  `take_while`, `drop_while`, `chunk`, `sized_chunk`, `intersperse`,
  `interleave`, `reduce`, `any`, `all`, `empty`, `once` and `single` functions.
- Breaking change in `iterator.take`. Now it returns an iterator instead of a
  list.
- The `string` module gains the `crop` function.

## v0.14.0 - 2021-02-18

- The `list` modules gains the `fold_until`, `window`, and `window_by_2`
  functions.
- The `int` module gains the `clamp` function.
- The `float` module gains the `clamp` function.
- The `io` module gains the `get_line` function.

## v0.13.0 - 2021-01-13

- The `int` module gains the `absolute_value`, `sum` and `product` functions.
- The `float` module gains the `sum` and `product` functions.
- The `result` module gains the `lazy_or`, `lazy_unwrap`, and `replace_error`
  functions.
- The `bool` module gains the `nand`, `nor`, `exclusive_nor`, and `exclusive_or`
  functions.
- The `bit_builder` module gains the `from_string_builder` function.
- The `list` modules gains the `index_fold`, `permutations`, and `try_fold`
  functions.
- Breaking change in `queue.from_list`. The head element in the list becomes the
  first element in the queue.
- Fix `queue.pop_back` and `queue.pop_front`

## v0.12.0 - 2020-11-04

- The `function` module gains `curry2` to `curry6`.
- The `list` module gains the `each`, and `partition` functions.
- The `int` and `float` modules gain the `negate` function.
- The `int` module gains the `to_float` function.
- The `result` module gains the `all` function.
- The `dynamic` module gains the `option`, `result` and `typed_result`
  functions.
- The `uri` module gains the `percent_encode` and `percent_decode` functions.
- The `os` module gains the `erlang_timestamp` function.
- The `iterator` module gains the `append`, `flatten`, `flat_map`, `step`,
  and `find` functions.

## v0.11.0 - 2020-08-22

- Fix `uri.parse_query` to handle the case where query parameters are present
  without a value.
- The types for `list.find_map` have been relaxed.
- The `dynamic.typed_list` argument label has changed from `containing` to
  `of`.
- The `dynamic` module gains the `any` function.
- The `bit_builder` module gains the `from_string` function.
- The `list` module gains the `key_set` and `unzip` function.
- The `function` module gains the `rescue` function.
- The `float` module gains the `power`, `square_root`, and `absolute_value`
  functions.

## v0.10.1 - 2020-07-01

- Fix `dynamic.string` to check that binary contains only utf8 characters.

## v0.10.0 - 2020-06-30

- `bit_string` module created with `from_string`, `byte_size`, `append`,
  `part`, `to_string`, `is_utf8`, `int_to_u32` and `int_from_u32` functions.
- The `bit_builder` module has been introduced with `prepend`, `append`,
  `prepend_builder`, `append_builder`, `prepend_string`, `append_string`,
  `concat`, `from_bit_string`, `to_bit_string`, and `byte_size` functions.
- The `iodata` module has been renamed to `string_builder`.
- `os` module created with `get_env`, `insert_env`, `delete_env` and
  `system_time`.
- The `string` module gains the `split_once` and `utf_codepoint` functions.
- The `dynamic` module gains the `bit_string` function.
- The `uri` module gains the `origin` and `merge` function.
- The `io.debug` function returns the printed term.
- The `dynamic.list` function has been renamed to `dynamic.typed_list`.
- The `dynamic.opaque_list` function has been renamed to `dynamic.list`.
- The `dynamic.tuple2_of` function has been renamed to `dynamic.typed_tuple2`.
- The `list.traverse` function has been renamed to `list.try_map`.
- The `list.traverse` first argument gains the label `over`.
- The `option` module gains the the `map`, `flatten`, `then` and `or`
  functions.
- The `result` module gains the the `or` function.
- Created the `regex` module with the `from_string`, `compile`, `check`,
  `split` and `scan` functions.
- The `list` module gains the the `pop`, `pop_map` and `key_pop` functions.
- `base` module created with `encode64`, `decode64`, `url_encode64` and
  `url_decode64`.

## v0.9.0 - 2020-05-26

- Created the `iterator` module with the `unfold`, `repeatedly`, `repeat`,
  `from_list`, `fold`, `run`, `to_list`, `take`, `drop`, `map`, `filter`,
  `cycle`, and `range` functions.
- Created the `set` module with the `new`, `insert`, `delete`, `to_list`,
  `from_list`, `fold`, `take`, `union`, `intersection`, and `contains`
  functions.
- Created the `io` module with the `print`, `println`, and `debug` functions.
- Created the `queue` module with the `new`, `from_list`, `to_list`,
  `is_empty`, `length`, `push_back`, `push_front`, `pop_back`, `pop_front`,
  `reverse`, `is_logically_equal`, and `is_equal` functions.
- Created the `option` module containing the `Option` type and the `is_some`
  and `is_none` functions.
- Created the `option` module containing the `Option` type and the `is_some`,
  `is_none`, `to_result`, `from_result` and `unwrap` functions.
- Removed the `Option` alias and the `none` function from the `result` module.
- The `result` module gains the `nil_error` function.
- The `string` module gains `trim`, `trim_left`, `trim_right`, `starts_with`,
  `ends_with`, `slice`, `pad_left`, `pad_right` `drop_left`, `drop_right`,
  `pop_grapheme` and `to_graphemes` functions.
- `uri` module created with `parse`, `parse_query`, `path_segments`,
  `query_to_string` and `to_string`.
- The `dynamic` module gains the `map`, `opaque_list`, `tuple2`, and
  `tuple2_of` functions.
- The `list` module gains the `filter_map` function.
- The `list.contains` label `has` has been changed to `any`.
- The `list.sort` label `sort_by` has been changed to `by`.
- The `list.fold`'s first argument gained the label `over`.
- The `map.fold`'s first argument gained the label `over`.
- The `map.take`'s `drop` argument has been changed to `keeping`.

## v0.8.0 - 2020-04-28

- The error type for `atom.from_string` has been renamed to `FromStringError`.
- The `string` module gains `contains` and `repeat` functions.
- The `expect` module has been renamed to `should`. Functions in the module
  starting with `is_` have been changed to `be_`.
- The `string.replace` and `iodata.replace` `all` argument label has been
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
- The `float` module gains the `max`, and `min` functions.
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

import {
  Ok,
  Error,
  List,
  BitString,
  UtfCodepoint,
  toBitString,
  stringBits,
} from "./gleam.js";
import {
  CompileError as RegexCompileError,
  Match as RegexMatch,
} from "./gleam/regex.js";
import { Some, None } from "./gleam/option.js";

const HASHCODE_CACHE = new WeakMap();

const Nil = undefined;

export function identity(x) {
  return x;
}

export function parse_int(value) {
  if (/^[-+]?(\d+)$/.test(value)) {
    return new Ok(parseInt(value));
  } else {
    return new Error(Nil);
  }
}

export function parse_float(value) {
  if (/^[-+]?(\d+)\.(\d+)$/.test(value)) {
    return new Ok(parseFloat(value));
  } else {
    return new Error(Nil);
  }
}

export function to_string(term) {
  return term.toString();
}

export function float_to_string(float) {
  let string = float.toString();
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    return string + ".0";
  }
}

export function int_to_base_string(int, base) {
  return int.toString(base);
}

export function string_replace(string, target, substitute) {
  return string.replaceAll(target, substitute);
}

export function string_reverse(string) {
  return [...string].reverse().join("");
}

export function string_length(string) {
  let iterator = graphemes_iterator(string);
  if (iterator) {
    let i = 0;
    for (let _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string.match(/./gu).length;
  }
}

function graphemes_iterator(string) {
  if (Intl && Intl.Segmenter) {
    return new Intl.Segmenter("en-gb").segment(string)[Symbol.iterator]();
  }
}

export function pop_grapheme(string) {
  let first;
  let iterator = graphemes_iterator(string);
  if (iterator) {
    first = iterator.next().value?.segment;
  } else {
    first = string.match(/./u)?.[0];
  }
  if (first) {
    return new Ok([first, string.slice(first.length)]);
  } else {
    return new Error(Nil);
  }
}

export function lowercase(string) {
  return string.toLowerCase();
}

export function uppercase(string) {
  return string.toUpperCase();
}

export function less_than(a, b) {
  return a < b;
}

export function add(a, b) {
  return a + b;
}

export function equal(a, b) {
  return a === b;
}

export function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}

export function join(xs) {
  return xs.toArray().join("");
}

export function length(data) {
  return data.length;
}

export function slice_string(string, from, length) {
  return string.slice(from, from + length);
}

export function crop_string(string, substring) {
  return string.substring(string.indexOf(substring));
}

export function index_of(haystack, needle) {
  return haystack.indexOf(needle) | 0;
}

export function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}

export function ends_with(haystack, needle) {
  return haystack.endsWith(needle);
}

export function split_once(haystack, needle) {
  let index = haystack.indexOf(needle);
  if (index >= 0) {
    let before = haystack.slice(0, index);
    let after = haystack.slice(index + needle.length);
    return new Ok([before, after]);
  } else {
    return new Error(Nil);
  }
}

export function trim(string) {
  return string.trim();
}

export function trim_left(string) {
  return string.trimLeft();
}

export function trim_right(string) {
  return string.trimRight();
}

export function bit_string_from_string(string) {
  return new toBitString([stringBits(string)]);
}

export function bit_string_concat(bit_strings) {
  return toBitString(bit_strings.toArray().map((b) => b.buffer));
}

export function log(term) {
  console.log(term);
}

export function crash(message) {
  throw new globalThis.Error(message);
}

export function bit_string_to_string(bit_string) {
  try {
    let decoder = new TextDecoder("utf-8", { fatal: true });
    return new Ok(decoder.decode(bit_string.buffer));
  } catch (_error) {
    return new Error(undefined);
  }
}

export function print(string) {
  if (typeof process === "object") {
    process.stdout.write(string); // We can write without a trailing newline
  } else {
    console.log(string); // We're in a browser. Newlines are mandated
  }
}

export function ceiling(float) {
  return Math.ceil(float);
}

export function floor(float) {
  return Math.floor(float);
}

export function round(float) {
  return Math.round(float);
}

export function truncate(float) {
  return Math.trunc(float);
}

export function power(base, exponent) {
  return Math.pow(base, exponent);
}

export function bit_string_slice(bits, position, length) {
  let start = Math.min(position, position + length);
  let end = Math.max(position, position + length);
  if (start < 0 || end > bits.length) return new Error(Nil);
  let buffer = new Uint8Array(bits.buffer.buffer, start, Math.abs(length));
  return new Ok(new BitString(buffer));
}

export function codepoint(int) {
  return new UtfCodepoint(int);
}

export function regex_check(regex, string) {
  return regex.test(string);
}

export function compile_regex(pattern, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive) flags += "i";
    if (options.multi_line) flags += "m";
    return new Ok(new RegExp(pattern, flags));
  } catch (error) {
    let number = (error.columnNumber || 0) | 0;
    return new Error(new RegexCompileError(error.message, number));
  }
}

export function regex_scan(regex, string) {
  let matches = Array.from(string.matchAll(regex)).map((match) => {
    let content = match.shift();
    let submatches = match.map((x) => (x ? new Some(x) : new None()));
    return new RegexMatch(content, List.fromArray(submatches));
  });
  return List.fromArray(matches);
}

export function hashcode(obj) {
  let existing = HASHCODE_CACHE.get(obj);
  if (existing) {
    return existing;
  } else if (obj instanceof Object) {
    let hashcode = JSON.stringify(obj);
    HASHCODE_CACHE.set(obj, hashcode);
    return hashcode;
  } else {
    return obj.toString();
  }
}

export function new_map() {
  return new Map();
}

export function map_size(map) {
  return map.size;
}

export function map_to_list(map) {
  return List.fromArray([...map.values()]);
}

export function map_from_list(list) {
  let map = new Map();
  for (let pair of list) {
    map.set(hashcode(pair[0]), pair);
  }
  return map;
}

export function map_has_key(k, map) {
  return map.has(hashcode(k));
}

export function map_remove(k, map) {
  const result = new Map(map);
  result.delete(hashcode(k));
  return result;
}

export function map_filter(f, map) {
  const result = new Map();
  for (let [hash, [k, v]] of map) {
    if (f(k)) result.set(hash, [k, v]);
  }
  return result;
}

export function map_get(from, get) {
  const entry = from.get(hashcode(get));
  if (entry) {
    return new Ok(entry[1]);
  } else {
    return new Error(Nil);
  }
}

export function map_insert(key, value, map) {
  return new Map(map).set(hashcode(key), [key, value]);
}

export function map_keys(map) {
  return List.fromArray([...map.values()].map(([key, value]) => key));
}

export function map_values(map) {
  return List.fromArray([...map.values()].map(([key, value]) => value));
}

export function map_map_values(fn, map) {
  const result = new Map();
  for (let [hash, [k, v]] of map) result.set(hash, [k, fn(k, v)]);
  return result;
}

export function map_merge(into, merge) {
  return new Map([...into, ...merge]);
}

export function map_take(keys, map) {
  const result = new Map();
  keys.toArray().forEach((key) => {
    const hash = hashcode(key);
    const keyValue = map.get(hash);
    if (keyValue !== undefined) {
      result.set(hash, keyValue);
    }
  });
  return result;
}

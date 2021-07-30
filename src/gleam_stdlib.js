const Nil = undefined;

function to_list(array) {
  let list = [];
  for (let item of array.reverse()) {
    list = [item, list];
  }
  return list;
}

function gleam_ok(x) {
  return { type: "Ok", 0: x };
}

function gleam_error(x) {
  return { type: "Error", 0: x };
}

export function identity(x) {
  return x;
}

export function parse_int(value) {
  if (/^[-+]?(\d+)$/.test(value)) {
    return gleam_ok(Number(value));
  } else {
    return gleam_error(Nil);
  }
}

export function to_string(int) {
  return int.toString();
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
    return gleam_ok([first, string.slice(first.length)]);
  } else {
    return gleam_error(Nil);
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
  return to_list(xs.split(pattern));
}

export function join(xs) {
  return xs.flat(Infinity).join("");
}

export function byte_size(data) {
  if (data instanceof ArrayBuffer) {
    return data.byteLength;
  } else if (typeof Blob === "function") {
    return new Blob([data]).size;
  } else if (typeof Buffer === "function") {
    return Buffer.byteLength(data);
  } else {
    return data.length;
  }
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
    return gleam_ok([before, after]);
  } else {
    return gleam_error(Nil);
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
  return new TextEncoder().encode(string).buffer;
}

export function bit_string_append(first, second) {
  let array = new Uint8Array(first.byteLength + second.byteLength);
  array.set(new Uint8Array(first), 0);
  array.set(new Uint8Array(second), first.byteLength);
  return array.buffer;
}

export function log(term) {
  console.log(term);
}

export function stringify(data) {
  let replacer = (_key, value) =>
    typeof value === "bigint" ? value.toString() + "n" : value;
  try {
    return JSON.stringify(data, replacer);
  } catch (_error) {
    return "//reference-cycle";
  }
}

export function crash(message) {
  throw new Error(message);
}

export function bit_string_to_string(bit_string) {
  try {
    return gleam_ok(
      new TextDecoder("utf-8", { fatal: true }).decode(bit_string)
    );
  } catch (_error) {
    return gleam_error(undefined);
  }
}

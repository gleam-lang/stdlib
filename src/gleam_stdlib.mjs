import {
  BitArray,
  Error,
  List,
  Ok,
  Result,
  UtfCodepoint,
  stringBits,
  toBitArray,
  bitArraySlice,
  NonEmpty,
  Empty,
  CustomType,
} from "./gleam.mjs";
import { Some, None } from "./gleam/option.mjs";
import Dict from "./dict.mjs";
import { classify } from "./gleam/dynamic.mjs";
import { DecodeError } from "./gleam/dynamic/decode.mjs";

const Nil = undefined;
const NOT_FOUND = {};

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
  if (/^[-+]?(\d+)\.(\d+)([eE][-+]?\d+)?$/.test(value)) {
    return new Ok(parseFloat(value));
  } else {
    return new Error(Nil);
  }
}

export function to_string(term) {
  return term.toString();
}

export function int_to_base_string(int, base) {
  return int.toString(base).toUpperCase();
}

const int_base_patterns = {
  2: /[^0-1]/,
  3: /[^0-2]/,
  4: /[^0-3]/,
  5: /[^0-4]/,
  6: /[^0-5]/,
  7: /[^0-6]/,
  8: /[^0-7]/,
  9: /[^0-8]/,
  10: /[^0-9]/,
  11: /[^0-9a]/,
  12: /[^0-9a-b]/,
  13: /[^0-9a-c]/,
  14: /[^0-9a-d]/,
  15: /[^0-9a-e]/,
  16: /[^0-9a-f]/,
  17: /[^0-9a-g]/,
  18: /[^0-9a-h]/,
  19: /[^0-9a-i]/,
  20: /[^0-9a-j]/,
  21: /[^0-9a-k]/,
  22: /[^0-9a-l]/,
  23: /[^0-9a-m]/,
  24: /[^0-9a-n]/,
  25: /[^0-9a-o]/,
  26: /[^0-9a-p]/,
  27: /[^0-9a-q]/,
  28: /[^0-9a-r]/,
  29: /[^0-9a-s]/,
  30: /[^0-9a-t]/,
  31: /[^0-9a-u]/,
  32: /[^0-9a-v]/,
  33: /[^0-9a-w]/,
  34: /[^0-9a-x]/,
  35: /[^0-9a-y]/,
  36: /[^0-9a-z]/,
};

export function int_from_base_string(string, base) {
  if (int_base_patterns[base].test(string.replace(/^-/, "").toLowerCase())) {
    return new Error(Nil);
  }

  const result = parseInt(string, base);

  if (isNaN(result)) {
    return new Error(Nil);
  }

  return new Ok(result);
}

export function string_replace(string, target, substitute) {
  return string.replaceAll(target, substitute);
}

export function string_reverse(string) {
  return [...string].reverse().join("");
}

export function string_length(string) {
  if (string === "") {
    return 0;
  }
  const iterator = graphemes_iterator(string);
  if (iterator) {
    let i = 0;
    for (const _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string.match(/./gsu).length;
  }
}

export function graphemes(string) {
  const iterator = graphemes_iterator(string);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string.match(/./gsu));
  }
}

let segmenter = undefined;

function graphemes_iterator(string) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string)[Symbol.iterator]();
  }
}

export function pop_grapheme(string) {
  let first;
  const iterator = graphemes_iterator(string);
  if (iterator) {
    first = iterator.next().value?.segment;
  } else {
    first = string.match(/./su)?.[0];
  }
  if (first) {
    return new Ok([first, string.slice(first.length)]);
  } else {
    return new Error(Nil);
  }
}

export function pop_codeunit(str) {
  return [str.charCodeAt(0) | 0, str.slice(1)];
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

export function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}

export function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}

export function length(data) {
  return data.length;
}

export function string_byte_slice(string, index, length) {
  return string.slice(index, index + length);
}

export function string_grapheme_slice(string, idx, len) {
  if (len <= 0 || idx >= string.length) {
    return "";
  }

  const iterator = graphemes_iterator(string);
  if (iterator) {
    while (idx-- > 0) {
      iterator.next();
    }

    let result = "";

    while (len-- > 0) {
      const v = iterator.next().value;
      if (v === undefined) {
        break;
      }

      result += v.segment;
    }

    return result;
  } else {
    return string
      .match(/./gsu)
      .slice(idx, idx + len)
      .join("");
  }
}

export function string_codeunit_slice(str, from, length) {
  return str.slice(from, from + length);
}
export function crop_string(string, substring) {
  return string.substring(string.indexOf(substring));
}

export function contains_string(haystack, needle) {
  return haystack.indexOf(needle) >= 0;
}

export function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}

export function ends_with(haystack, needle) {
  return haystack.endsWith(needle);
}

export function split_once(haystack, needle) {
  const index = haystack.indexOf(needle);
  if (index >= 0) {
    const before = haystack.slice(0, index);
    const after = haystack.slice(index + needle.length);
    return new Ok([before, after]);
  } else {
    return new Error(Nil);
  }
}

const unicode_whitespaces = [
  "\u0020", // Space
  "\u0009", // Horizontal tab
  "\u000A", // Line feed
  "\u000B", // Vertical tab
  "\u000C", // Form feed
  "\u000D", // Carriage return
  "\u0085", // Next line
  "\u2028", // Line separator
  "\u2029", // Paragraph separator
].join("");

const trim_start_regex = /* @__PURE__ */ new RegExp(
  `^[${unicode_whitespaces}]*`,
);
const trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);

export function trim_start(string) {
  return string.replace(trim_start_regex, "");
}

export function trim_end(string) {
  return string.replace(trim_end_regex, "");
}

export function bit_array_from_string(string) {
  return toBitArray([stringBits(string)]);
}

export function bit_array_bit_size(bit_array) {
  return bit_array.bitSize;
}

export function bit_array_byte_size(bit_array) {
  return bit_array.byteSize;
}

export function bit_array_pad_to_bytes(bit_array) {
  const trailingBitsCount = bit_array.bitSize % 8;

  // If the bit array is a whole number of bytes it can be returned unchanged
  if (trailingBitsCount === 0) {
    return bit_array;
  }

  const finalByte = bit_array.byteAt(bit_array.byteSize - 1);

  // The required final byte has its unused trailing bits set to zero
  const unusedBitsCount = 8 - trailingBitsCount;
  const correctFinalByte = (finalByte >> unusedBitsCount) << unusedBitsCount;

  // If the unused bits in the final byte are already set to zero then the
  // existing buffer can be re-used, avoiding a copy
  if (finalByte === correctFinalByte) {
    return new BitArray(
      bit_array.rawBuffer,
      bit_array.byteSize * 8,
      bit_array.bitOffset,
    );
  }

  // Copy the bit array into a new aligned buffer and set the correct final byte
  const buffer = new Uint8Array(bit_array.byteSize);
  for (let i = 0; i < buffer.length - 1; i++) {
    buffer[i] = bit_array.byteAt(i);
  }
  buffer[buffer.length - 1] = correctFinalByte;

  return new BitArray(buffer);
}

export function bit_array_concat(bit_arrays) {
  return toBitArray(bit_arrays.toArray());
}

export function console_log(term) {
  console.log(term);
}

export function console_error(term) {
  console.error(term);
}

export function crash(message) {
  throw new globalThis.Error(message);
}

export function bit_array_to_string(bit_array) {
  // If the bit array isn't a whole number of bytes then return an error
  if (bit_array.bitSize % 8 !== 0) {
    return new Error(Nil);
  }

  try {
    const decoder = new TextDecoder("utf-8", { fatal: true });

    if (bit_array.bitOffset === 0) {
      return new Ok(decoder.decode(bit_array.rawBuffer));
    } else {
      // The input data isn't aligned, so copy it into a new aligned buffer so
      // that TextDecoder can be used
      const buffer = new Uint8Array(bit_array.byteSize);
      for (let i = 0; i < buffer.length; i++) {
        buffer[i] = bit_array.byteAt(i);
      }
      return new Ok(decoder.decode(buffer));
    }
  } catch {
    return new Error(Nil);
  }
}

export function print(string) {
  if (typeof process === "object" && process.stdout?.write) {
    process.stdout.write(string); // We can write without a trailing newline
  } else if (typeof Deno === "object") {
    Deno.stdout.writeSync(new TextEncoder().encode(string)); // We can write without a trailing newline
  } else {
    console.log(string); // We're in a browser. Newlines are mandated
  }
}

export function print_error(string) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string); // We can write without a trailing newline
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string)); // We can write without a trailing newline
  } else {
    console.error(string); // We're in a browser. Newlines are mandated
  }
}

export function print_debug(string) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string + "\n"); // If we're in Node.js, use `stderr`
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string + "\n")); // If we're in Deno, use `stderr`
  } else {
    console.log(string); // Otherwise, use `console.log` (so that it doesn't look like an error)
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
  // It is checked in Gleam that:
  // - The base is non-negative and that the exponent is not fractional.
  // - The base is non-zero and the exponent is non-negative (otherwise
  //   the result will essentially be division by zero).
  // It can thus be assumed that valid input is passed to the Math.pow
  // function and a NaN or Infinity value will not be produced.
  return Math.pow(base, exponent);
}

export function random_uniform() {
  const random_uniform_result = Math.random();
  // With round-to-nearest-even behavior, the ranges claimed for the functions below
  // (excluding the one for Math.random() itself) aren't exact.
  // If extremely large bounds are chosen (2^53 or higher),
  // it's possible in extremely rare cases to calculate the usually-excluded upper bound.
  // Note that as numbers in JavaScript are IEEE 754 floating point numbers
  // See: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random>
  // Because of this, we just loop 'until' we get a valid result where 0.0 <= x < 1.0:
  if (random_uniform_result === 1.0) {
    return random_uniform();
  }
  return random_uniform_result;
}

export function bit_array_slice(bits, position, length) {
  const start = Math.min(position, position + length);
  const end = Math.max(position, position + length);

  if (start < 0 || end * 8 > bits.bitSize) {
    return new Error(Nil);
  }

  return new Ok(bitArraySlice(bits, start * 8, end * 8));
}

export function codepoint(int) {
  return new UtfCodepoint(int);
}

export function string_to_codepoint_integer_list(string) {
  return List.fromArray(Array.from(string).map((item) => item.codePointAt(0)));
}

export function utf_codepoint_list_to_string(utf_codepoint_integer_list) {
  return utf_codepoint_integer_list
    .toArray()
    .map((x) => String.fromCodePoint(x.value))
    .join("");
}

export function utf_codepoint_to_int(utf_codepoint) {
  return utf_codepoint.value;
}

export function new_map() {
  return Dict.new();
}

export function map_size(map) {
  return map.size;
}

export function map_to_list(map) {
  return List.fromArray(map.entries());
}

export function map_remove(key, map) {
  return map.delete(key);
}

export function map_get(map, key) {
  const value = map.get(key, NOT_FOUND);
  if (value === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value);
}

export function map_insert(key, value, map) {
  return map.set(key, value);
}

function unsafe_percent_decode(string) {
  return decodeURIComponent(string || "");
}

function unsafe_percent_decode_query(string) {
  return decodeURIComponent((string || "").replace("+", " "));
}

export function percent_decode(string) {
  try {
    return new Ok(unsafe_percent_decode(string));
  } catch {
    return new Error(Nil);
  }
}

export function percent_encode(string) {
  return encodeURIComponent(string).replace("%2B", "+");
}

export function parse_query(query) {
  try {
    const pairs = [];
    for (const section of query.split("&")) {
      const [key, value] = section.split("=");
      if (!key) continue;

      const decodedKey = unsafe_percent_decode_query(key);
      const decodedValue = unsafe_percent_decode_query(value);
      pairs.push([decodedKey, decodedValue]);
    }
    return new Ok(List.fromArray(pairs));
  } catch {
    return new Error(Nil);
  }
}

const b64EncodeLookup = [
  65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
  84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
  107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
  122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47,
];

let b64TextDecoder;

// Implementation based on https://github.com/mitschabaude/fast-base64/blob/main/js.js
export function base64_encode(bit_array, padding) {
  b64TextDecoder ??= new TextDecoder();

  bit_array = bit_array_pad_to_bytes(bit_array);

  const m = bit_array.byteSize;
  const k = m % 3;
  const n = Math.floor(m / 3) * 4 + (k && k + 1);
  const N = Math.ceil(m / 3) * 4;
  const encoded = new Uint8Array(N);

  for (let i = 0, j = 0; j < m; i += 4, j += 3) {
    const y =
      (bit_array.byteAt(j) << 16) +
      (bit_array.byteAt(j + 1) << 8) +
      (bit_array.byteAt(j + 2) | 0);

    encoded[i] = b64EncodeLookup[y >> 18];
    encoded[i + 1] = b64EncodeLookup[(y >> 12) & 0x3f];
    encoded[i + 2] = b64EncodeLookup[(y >> 6) & 0x3f];
    encoded[i + 3] = b64EncodeLookup[y & 0x3f];
  }

  let base64 = b64TextDecoder.decode(new Uint8Array(encoded.buffer, 0, n));

  if (padding) {
    if (k === 1) {
      base64 += "==";
    } else if (k === 2) {
      base64 += "=";
    }
  }

  return base64;
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64
export function base64_decode(sBase64) {
  try {
    const binString = atob(sBase64);
    const length = binString.length;
    const array = new Uint8Array(length);
    for (let i = 0; i < length; i++) {
      array[i] = binString.charCodeAt(i);
    }
    return new Ok(new BitArray(array));
  } catch {
    return new Error(Nil);
  }
}

export function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Array`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Nil";
  } else if (data === undefined) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}

export function byte_size(string) {
  return new TextEncoder().encode(string).length;
}

// In JavaScript bitwise operations convert numbers to a sequence of 32 bits
// while Erlang uses arbitrary precision.
// To get around this problem and get consistent results use BigInt and then
// downcast the value back to a Number value.

export function bitwise_and(x, y) {
  return Number(BigInt(x) & BigInt(y));
}

export function bitwise_not(x) {
  return Number(~BigInt(x));
}

export function bitwise_or(x, y) {
  return Number(BigInt(x) | BigInt(y));
}

export function bitwise_exclusive_or(x, y) {
  return Number(BigInt(x) ^ BigInt(y));
}

export function bitwise_shift_left(x, y) {
  return Number(BigInt(x) << BigInt(y));
}

export function bitwise_shift_right(x, y) {
  return Number(BigInt(x) >> BigInt(y));
}

export function inspect(v) {
  return new Inspector().inspect(v);
}

export function float_to_string(float) {
  const string = float.toString().replace("+", "");
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    const index = string.indexOf("e");
    if (index >= 0) {
      return string.slice(0, index) + ".0" + string.slice(index);
    } else {
      return string + ".0";
    }
  }
}

class Inspector {
  #references = new Set();

  inspect(v) {
    const t = typeof v;
    if (v === true) return "True";
    if (v === false) return "False";
    if (v === null) return "//js(null)";
    if (v === undefined) return "Nil";
    if (t === "string") return this.#string(v);
    if (t === "bigint" || Number.isInteger(v)) return v.toString();
    if (t === "number") return float_to_string(v);
    if (v instanceof UtfCodepoint) return this.#utfCodepoint(v);
    if (v instanceof BitArray) return this.#bit_array(v);
    if (v instanceof RegExp) return `//js(${v})`;
    if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
    if (v instanceof globalThis.Error) return `//js(${v.toString()})`;
    if (v instanceof Function) {
      const args = [];
      for (const i of Array(v.length).keys())
        args.push(String.fromCharCode(i + 97));
      return `//fn(${args.join(", ")}) { ... }`;
    }

    if (this.#references.size === this.#references.add(v).size) {
      return "//js(circular reference)";
    }

    let printed;
    if (Array.isArray(v)) {
      printed = `#(${v.map((v) => this.inspect(v)).join(", ")})`;
    } else if (v instanceof List) {
      printed = this.#list(v);
    } else if (v instanceof CustomType) {
      printed = this.#customType(v);
    } else if (v instanceof Dict) {
      printed = this.#dict(v);
    } else if (v instanceof Set) {
      return `//js(Set(${[...v].map((v) => this.inspect(v)).join(", ")}))`;
    } else {
      printed = this.#object(v);
    }
    this.#references.delete(v);
    return printed;
  }

  #object(v) {
    const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
    const props = [];
    for (const k of Object.keys(v)) {
      props.push(`${this.inspect(k)}: ${this.inspect(v[k])}`);
    }
    const body = props.length ? " " + props.join(", ") + " " : "";
    const head = name === "Object" ? "" : name + " ";
    return `//js(${head}{${body}})`;
  }

  #dict(map) {
    let body = "dict.from_list([";
    let first = true;
    map.forEach((value, key) => {
      if (!first) body = body + ", ";
      body = body + "#(" + this.inspect(key) + ", " + this.inspect(value) + ")";
      first = false;
    });
    return body + "])";
  }

  #customType(record) {
    const props = Object.keys(record)
      .map((label) => {
        const value = this.inspect(record[label]);
        return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
      })
      .join(", ");
    return props
      ? `${record.constructor.name}(${props})`
      : record.constructor.name;
  }

  #list(list) {
    if (list instanceof Empty) {
      return "[]";
    }

    let char_out = 'charlist.from_string("';
    let list_out = "[";

    let current = list;
    while (current instanceof NonEmpty) {
      let element = current.head;
      current = current.tail;

      if (list_out !== "[") {
        list_out += ", ";
      }
      list_out += this.inspect(element);

      if (char_out) {
        if (Number.isInteger(element) && element >= 32 && element <= 126) {
          char_out += String.fromCharCode(element);
        } else {
          char_out = null;
        }
      }
    }

    if (char_out) {
      return char_out + '")';
    } else {
      return list_out + "]";
    }
  }

  #string(str) {
    let new_str = '"';
    for (let i = 0; i < str.length; i++) {
      const char = str[i];
      switch (char) {
        case "\n":
          new_str += "\\n";
          break;
        case "\r":
          new_str += "\\r";
          break;
        case "\t":
          new_str += "\\t";
          break;
        case "\f":
          new_str += "\\f";
          break;
        case "\\":
          new_str += "\\\\";
          break;
        case '"':
          new_str += '\\"';
          break;
        default:
          if (char < " " || (char > "~" && char < "\u{00A0}")) {
            new_str +=
              "\\u{" +
              char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") +
              "}";
          } else {
            new_str += char;
          }
      }
    }
    new_str += '"';
    return new_str;
  }

  #utfCodepoint(codepoint) {
    return `//utfcodepoint(${String.fromCodePoint(codepoint.value)})`;
  }

  #bit_array(bits) {
    if (bits.bitSize === 0) {
      return "<<>>";
    }

    let acc = "<<";

    for (let i = 0; i < bits.byteSize - 1; i++) {
      acc += bits.byteAt(i).toString();
      acc += ", ";
    }

    if (bits.byteSize * 8 === bits.bitSize) {
      acc += bits.byteAt(bits.byteSize - 1).toString();
    } else {
      const trailingBitsCount = bits.bitSize % 8;
      acc += bits.byteAt(bits.byteSize - 1) >> (8 - trailingBitsCount);
      acc += `:size(${trailingBitsCount})`;
    }

    acc += ">>";
    return acc;
  }
}

export function base16_encode(bit_array) {
  const trailingBitsCount = bit_array.bitSize % 8;

  let result = "";

  for (let i = 0; i < bit_array.byteSize; i++) {
    let byte = bit_array.byteAt(i);

    if (i === bit_array.byteSize - 1 && trailingBitsCount !== 0) {
      const unusedBitsCount = 8 - trailingBitsCount;
      byte = (byte >> unusedBitsCount) << unusedBitsCount;
    }

    result += byte.toString(16).padStart(2, "0").toUpperCase();
  }

  return result;
}

export function base16_decode(string) {
  const bytes = new Uint8Array(string.length / 2);
  for (let i = 0; i < string.length; i += 2) {
    const a = parseInt(string[i], 16);
    const b = parseInt(string[i + 1], 16);
    if (isNaN(a) || isNaN(b)) return new Error(Nil);
    bytes[i / 2] = a * 16 + b;
  }
  return new Ok(new BitArray(bytes));
}

export function bit_array_to_int_and_size(bits) {
  const trailingBitsCount = bits.bitSize % 8;
  const unusedBitsCount = trailingBitsCount === 0 ? 0 : 8 - trailingBitsCount;

  return [bits.byteAt(0) >> unusedBitsCount, bits.bitSize];
}

export function bit_array_starts_with(bits, prefix) {
  if (prefix.bitSize > bits.bitSize) {
    return false;
  }

  // Check any whole bytes
  const byteCount = Math.trunc(prefix.bitSize / 8);
  for (let i = 0; i < byteCount; i++) {
    if (bits.byteAt(i) !== prefix.byteAt(i)) {
      return false;
    }
  }

  // Check any trailing bits at the end of the prefix
  if (prefix.bitSize % 8 !== 0) {
    const unusedBitsCount = 8 - (prefix.bitSize % 8);
    if (
      bits.byteAt(byteCount) >> unusedBitsCount !==
      prefix.byteAt(byteCount) >> unusedBitsCount
    ) {
      return false;
    }
  }

  return true;
}

export function log(x) {
  // It is checked in Gleam that:
  // - The input is strictly positive (x > 0)
  // - This ensures that Math.log will never return NaN or -Infinity
  // The function can thus safely pass the input to Math.log
  // and a valid finite float will always be produced.
  return Math.log(x);
}

export function exp(x) {
  return Math.exp(x);
}

export function list_to_array(list) {
  let current = list;
  let array = [];
  while (current instanceof NonEmpty) {
    array.push(current.head);
    current = current.tail;
  }
  return array;
}

export function index(data, key) {
  // Dictionaries and dictionary-like objects can be indexed
  if (data instanceof Dict || data instanceof WeakMap || data instanceof Map) {
    const token = {};
    const entry = data.get(key, token);
    if (entry === token) return new Ok(new None());
    return new Ok(new Some(entry));
  }

  const key_is_int = Number.isInteger(key);

  // Only elements 0-7 of lists can be indexed, negative indices are not allowed
  if (key_is_int && key >= 0 && key < 8 && data instanceof List) {
    let i = 0;
    for (const value of data) {
      if (i === key) return new Ok(new Some(value));
      i++;
    }
    return new Error("Indexable");
  }

  // Arrays and objects can be indexed
  if (
    (key_is_int && Array.isArray(data)) ||
    (data && typeof data === "object") ||
    (data && Object.getPrototypeOf(data) === Object.prototype)
  ) {
    if (key in data) return new Ok(new Some(data[key]));
    return new Ok(new None());
  }

  return new Error(key_is_int ? "Indexable" : "Dict");
}

export function list(data, decode, pushPath, index, emptyList) {
  if (!(data instanceof List || Array.isArray(data))) {
    const error = new DecodeError("List", classify(data), emptyList);
    return [emptyList, List.fromArray([error])];
  }

  const decoded = [];

  for (const element of data) {
    const layer = decode(element);
    const [out, errors] = layer;

    if (errors instanceof NonEmpty) {
      const [_, errors] = pushPath(layer, index.toString());
      return [emptyList, errors];
    }
    decoded.push(out);
    index++;
  }

  return [List.fromArray(decoded), emptyList];
}

export function dict(data) {
  if (data instanceof Dict) {
    return new Ok(data);
  }
  if (data instanceof Map || data instanceof WeakMap) {
    return new Ok(Dict.fromMap(data));
  }
  if (data == null) {
    return new Error("Dict");
  }
  if (typeof data !== "object") {
    return new Error("Dict");
  }
  const proto = Object.getPrototypeOf(data);
  if (proto === Object.prototype || proto === null) {
    return new Ok(Dict.fromObject(data));
  }
  return new Error("Dict");
}

export function bit_array(data) {
  if (data instanceof BitArray) return new Ok(data);
  if (data instanceof Uint8Array) return new Ok(new BitArray(data));
  return new Error(new BitArray(new Uint8Array()));
}

export function float(data) {
  if (typeof data === "number") return new Ok(data);
  return new Error(0.0);
}

export function int(data) {
  if (Number.isInteger(data)) return new Ok(data);
  return new Error(0);
}

export function string(data) {
  if (typeof data === "string") return new Ok(data);
  return new Error("");
}

export function is_null(data) {
  return data === null || data === undefined;
}

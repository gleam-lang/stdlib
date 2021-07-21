function to_list(array) {
  let list = [];
  for (let item of array.reverse()) {
    list = [item, list];
  }
  return list;
}

export function identity(x) {
  return x;
}

export function parse_int(value) {
  if (/^[-+]?(\d+)$/.test(value)) {
    return { type: "Ok", 0: Number(value) };
  } else {
    return { type: "Error", 0: null };
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
  return string.split("").reverse().join("");
}

export function string_length(string) {
  if (Intl && Intl.Segmenter) {
    let i = 0;
    for (let _ of new Intl.Segmenter("en-gb").segment(string)) {
      i++;
    }
    return i;
  } else {
    return string.match(/./gu).length;
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
  return xs.flat().join("");
}

export function byte_size(data) {
  if (typeof Blob === "function") {
    return new Blob([data]).size;
  } else if (typeof Buffer === "function") {
    return Buffer.byteLength(data);
  } else {
    return data.length;
  }
}

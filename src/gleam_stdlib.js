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

export function int_to_string(int) {
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

export function string_lowercase(string) {
  return string.toLowerCase();
}

export function string_uppercase(string) {
  return string.toUpperCase();
}

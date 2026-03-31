export function uint8array(list) {
  const ints = list.toArray();
  const array = new Uint8Array(ints.length);
  for (let i = 0; i < ints.length; i++) {
    array[i] = ints[i];
  }
  return array;
}

export function get_null() {
  return null;
}

export function object(items) {
  const object = {};
  for (const [k, v] of items) {
    object[k] = v;
  }
  return object;
}

export function map(items) {
  const object = new Map();
  for (const [k, v] of items) {
    object.set(k, v);
  }
  return object;
}

const singleton = { a: 1 };

export function singleton_object() {
  return singleton;
}

export function circular_reference() {
  const x = [1, 2, 3];
  x.push(x);
  return x;
}

export function js_error() {
  const error = new Error("Oh no!");
  error.name = "SomeError";
  return error;
}

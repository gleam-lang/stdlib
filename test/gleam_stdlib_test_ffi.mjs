export function uint8array(list) {
  let ints = list.toArray();
  let array = new Uint8Array(ints.length);
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

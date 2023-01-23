export function uint8array(list) {
  let ints = list.toArray();
  let array = new Uint8Array(ints.length);
  for (let i = 0; i < ints.length; i++) {
    array[i] = ints[i];
  }
  return array;
}

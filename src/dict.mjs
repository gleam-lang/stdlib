/**
 * This file uses jsdoc to annotate types.
 * These types can be checked using the typescript compiler with "checkjs" option.
 */

import { isEqual, Result$Error, Result$Ok } from "./gleam.mjs";

// -- HASH --------------------------------------------------------------------

const referenceMap = /* @__PURE__ */ new WeakMap();
const tempDataView = /* @__PURE__ */ new DataView(
  /* @__PURE__ */ new ArrayBuffer(8),
);
let referenceUID = 0;
/**
 * hash the object by reference using a weak map and incrementing uid
 * @param {any} o
 * @returns {number}
 */
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== undefined) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 0x7fffffff) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}

/**
 * merge two hashes in an order sensitive way
 * @param {number} a
 * @param {number} b
 * @returns {number}
 */
function hashMerge(a, b) {
  return (a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2))) | 0;
}

/**
 * standard string hash popularised by Java
 * @param {string} s
 * @returns {number}
 */
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = (Math.imul(31, hash) + s.charCodeAt(i)) | 0;
  }
  return hash;
}

/**
 * hash a number by converting to two integers and do some jumbling
 * @param {number} n
 * @returns {number}
 */
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(0x45d9f3b, (i >> 16) ^ i) ^ j;
}

/**
 * hash a BigInt by converting it to a string and hashing that
 * @param {BigInt} n
 * @returns {number}
 */
function hashBigInt(n) {
  return hashString(n.toString());
}

/**
 * hash any js object
 * @param {any} o
 * @returns {number}
 */
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {}
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = (Math.imul(31, h) + getHash(o[i])) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = (h + getHash(v)) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = (h + hashMerge(getHash(v), getHash(k))) | 0;
    });
  } else {
    const keys = Object.keys(o);
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      const v = o[k];
      h = (h + hashMerge(getHash(v), hashString(k))) | 0;
    }
  }
  return h;
}

/**
 * hash any js value
 * @param {any} u
 * @returns {number}
 */
export function getHash(u) {
  if (u === null) return 0x42108422;
  if (u === undefined) return 0x42108423;
  if (u === true) return 0x42108421;
  if (u === false) return 0x42108420;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0; // should be unreachable
  }
}

// -- DICT --------------------------------------------------------------------

/**
 * An implementation of the CHAMP data structure, an optimised HAMT.
 *
 * See: M. J. Steindorfer, J.J. Vinju (2015). Optimizing Hash-Array Mapped Tries for
 Fast and Lean Immutable JVM Collections. Available: https://michael.steindorfer.name/publications/oopsla15.pdf
 */
export default class Dict {
  constructor(size, root) {
    this.size = size;
    this.root = root;
  }
}

/// The power-of-2 branching factor for the dict. For example, a value of `5` indicates a 32-ary tree.
const bits = 5;
const mask = (1 << bits) - 1;

/// This symbol is used internally to avoid constructing results.
const noElementMarker = Symbol();

/// This symbol is used to store the "generation" on a node.
/// Using a symbol makes the property not enumerable, which means the generation
/// will be ignored during equality checks.
const generationKey = Symbol();

// Some commonly used constants throughout the code.
const emptyNode = /* @__PURE__ */ newNode(0);
const emptyDict = /* @__PURE__ */ new Dict(0, emptyNode);
const errorNil = /* @__PURE__ */ Result$Error(undefined);

function makeNode(generation, datamap, nodemap, data) {
  // The order of fields is important, as they define the order `isEqual` will
  // compare our fields. Putting the bitmaps first means that equality can
  // early-out if the bitmaps are not equal.
  return {
    // A node is a high-arity (32 in practice) hybrid tree node.
    // Hybrid means that it stores data directly as well as pointers to child nodes.
    //
    // Each node contains 2 bitmaps:
    // - The datamap has a bit set if that slot in the node contains direct data
    // - The nodemap has a bit set if that slot in the node contains another node.
    //
    // Both are exclusive to on another, so datamap & nodemap == 0.
    //
    // Every key/hash value directly correlates to a specific bit by using a trie
    // suffix (least significant bits first) encoding.
    // For example, if the last 5 bits of the hash are 1101, the bit to check for
    // that value is the 13th bit.
    datamap,
    nodemap,
    // The slots itself are stored in a single contiguous array that contains
    // both direct k/v-pairs and child nodes.
    //
    // The direct children come first, followed by the child nodes in _reverse order_:
    //
    //              7654321
    //     datamap: 1000100
    //     nodemap:   10011
    //     data: [key3, value3, key7, value7, child5, child2, child1]
    //            ------------------------->  <---------------------
    //                     datamap                    nodemap
    //
    // Every `1` bit in the datamap corresponds to a pair of [key, value] entries,
    // and every `1` bit in the nodemap corresponds to a child node entry.
    //
    // Children are stored in reverse order to avoid having to store or calculate an
    // "offset" value to skip over the direct children.
    data,
    // The generation is used to track which nodes need to be copied during transient updates.
    // Using a symbol here makes `isEqual` ignore this field.
    [generationKey]: generation,
  };
}

function newNode(generation) {
  return makeNode(generation, 0, 0, []);
}

/**
 * Copies a node and its data array if it's from another generation, making it safe
 * to mutate the node.
 */
function copyNode(node, generation) {
  if (node[generationKey] === generation) {
    return node;
  }

  const newData = node.data.slice(0);
  return makeNode(generation, node.datamap, node.nodemap, newData);
}

/**
 * Copies a node if needed and sets a new value.
 */
function copyAndSet(node, generation, idx, val) {
  if (node.data[idx] === val) {
    return node;
  }

  // Using copyNode is faster than a specialised implementation.
  node = copyNode(node, generation);
  node.data[idx] = val;
  return node;
}

/**
 * Copies a node if needed, and then inserts a new key-value pair.
 */
function copyAndInsertPair(node, generation, bit, idx, key, val) {
  const data = node.data;
  const length = data.length;

  // the fastest way to insert a pair is to always copy.
  const newData = new Array(length + 2);

  let readIndex = 0;
  let writeIndex = 0;

  while (readIndex < idx) newData[writeIndex++] = data[readIndex++];
  newData[writeIndex++] = key;
  newData[writeIndex++] = val;
  while (readIndex < length) newData[writeIndex++] = data[readIndex++];

  return makeNode(generation, node.datamap | bit, node.nodemap, newData);
}

function copyAndRemovePair(node, generation, bit, idx) {
  node = copyNode(node, generation);

  const data = node.data;
  const length = data.length;
  for (let w = idx, r = idx + 2; r < length; ++r, ++w) {
    data[w] = data[r];
  }
  data.pop();
  data.pop();

  node.datamap ^= bit;
  return node;
}

export function make() {
  return emptyDict;
}

export function from(iterable) {
  let transient = toTransient(emptyDict);
  for (const [key, value] of iterable) {
    transient = destructiveTransientInsert(key, value, transient);
  }
  return fromTransient(transient);
}

export function size(dict) {
  return dict.size;
}

export function get(dict, key) {
  const result = lookup(dict.root, key, getHash(key));
  return result !== noElementMarker ? Result$Ok(result) : errorNil;
}

export function has(dict, key) {
  return lookup(dict.root, key, getHash(key)) !== noElementMarker;
}

function lookup(node, key, hash) {
  for (let shift = 0; shift < 32; shift += bits) {
    const data = node.data;
    const bit = hashbit(hash, shift);

    if (node.nodemap & bit) {
      // we found our hash inside the nodemap, so we can continue our search there.
      node = data[data.length - 1 - index(node.nodemap, bit)];
    } else if (node.datamap & bit) {
      // we store this hash directly!
      //
      // this also means that there are no other values with the same
      // hash prefix in this dict.
      //
      // We still need to check if the key matches, but if it does we know for
      // sure that this is the correct value, and if it doesn't that we don't
      // contain the value in question.
      const dataidx = Math.imul(index(node.datamap, bit), 2);
      return isEqual(key, data[dataidx]) ? data[dataidx + 1] : noElementMarker;
    } else {
      // if the hash bit is not set in neither bitmaps, we immediately know that
      // this key cannot be inside this dict.
      return noElementMarker;
    }
  }

  // our shift has exceeded 32 bits. Everything that follows is
  // implicitely an overflow node and only contains direct children.
  const overflow = node.data;
  for (let i = 0; i < overflow.length; i += 2) {
    if (isEqual(key, overflow[i])) {
      return overflow[i + 1];
    }
  }

  return noElementMarker;
}

/**
 * We use "transient" values to allow for safer internal mutations of the data
 * structure. This is an optimisation only. No mutable API is exposed to the user.
 *
 * Transients are to be treated as having a linear (single-use, think rust) type.
 * A transient value becomes invalid as soon as it's passed to one of the functions.
 *
 * Internally, we track a "generation" value on each node. If the generation
 * doesn't match the one for the current transient, we have to copy - the node
 * could still be referenced by another dict instance!
 * After that, no other references than the transient one exists, so it's safe
 * to mutate in place.
 */
export function toTransient(dict) {
  return {
    generation: nextGeneration(dict),
    root: dict.root,
    size: dict.size,
    dict: dict,
  };
}

/**
 * Consume a transient, producing a normal Dict again.
 */
export function fromTransient(transient) {
  if (transient.root === transient.dict.root) {
    return transient.dict;
  }

  return new Dict(transient.size, transient.root);
}

/**
 * Find and allocate the next generation id.
 *
 * @template K,V
 * @param {Dict<K,V>} dict
 * @returns {number}
 */
function nextGeneration(dict) {
  const root = dict.root;
  if (root[generationKey] < Number.MAX_SAFE_INTEGER) {
    return root[generationKey] + 1;
  }

  // we have reached MAX_SAFE_INTEGER generations -
  // at this point, we have to walk the dictionary once to reset the counter
  // on every node. This is safe since it's part of the contract for transient
  // that only one of them exists at any given time.
  //
  const queue = [root];
  while (queue.length) {
    // order doesn't matter, so we can use push/pop for faster array usage.
    const node = queue.pop();

    // reset the generation to 0
    node[generationKey] = 0;

    // queue all other referenced nodes
    // We need to query the length from the nodemap, as we don't know if this
    //  is an overflow node or not! if it is, it will never have datamap set!
    const nodeStart = data.length - popcount(node.nodemap);
    for (let i = nodeStart; i < node.data.length; ++i) {
      queue.push(node.data[i]);
    }
  }

  return 1;
}

/// Insert is the second-most performance-sensitive operation.
/// We use a global "transient" value here to avoid doing a memory allocation.
const globalTransient = /* @__PURE__ */ toTransient(emptyDict);

export function insert(dict, key, value) {
  globalTransient.generation = nextGeneration(dict);
  globalTransient.size = dict.size;

  const hash = getHash(key);
  const root = insertIntoNode(globalTransient, dict.root, key, value, hash, 0);
  if (root === dict.root) {
    return dict;
  }

  return new Dict(globalTransient.size, root);
}

/**
 * Consume a transient, writing a new key/value pair into the dictionary it
 * represents. If the key already exists, it will be overwritten.
 *
 * Returns a new transient.
 */
export function destructiveTransientInsert(key, value, transient) {
  const hash = getHash(key);
  transient.root = insertIntoNode(transient, transient.root, key, value, hash, 0);
  return transient;
}

/**
 * Consume a transient, writing a new key/value pair if the key doesn't exist or updating
 * the existing value with a function if it does.
 *
 * Returns a new transient.
 */
export function destructiveTransientUpdateWith(key, fun, value, transient) {
  const hash = getHash(key);

  const existing = lookup(transient.root, key, hash);
  if (existing !== noElementMarker) {
    value = fun(existing);
  }
  transient.root = insertIntoNode(transient, transient.root, key, value, hash, 0);
  return transient;
}

function insertIntoNode(transient, node, key, value, hash, shift) {
  const data = node.data;
  const generation = transient.generation;

  // 1. Overflow Node
  // overflow nodes only contain key/value-pairs. we walk the data linearly trying to find a match.
  if (shift > 32) {
    for (let i = 0; i < data.length; i += 2) {
      if (isEqual(key, data[i])) {
        return copyAndSet(node, generation, i + 1, value);
      }
    }

    transient.size += 1;
    return copyAndInsertPair(node, generation, 0, data.length, key, value);
  }

  const bit = hashbit(hash, shift);

  // 2. Child Node
  // We have to check first if there is already a child node we have to traverse to.
  if (node.nodemap & bit) {
    const nodeidx = data.length - 1 - index(node.nodemap, bit);

    let child = data[nodeidx];
    child = insertIntoNode(transient, child, key, value, hash, shift + bits);
    return copyAndSet(node, generation, nodeidx, child);
  }

  // 3. New Data Node
  // No child node and no data node exists yet, so we can potentially just insert a new value.
  const dataidx = Math.imul(index(node.datamap, bit), 2);
  if ((node.datamap & bit) === 0) {
    transient.size += 1;
    return copyAndInsertPair(node, generation, bit, dataidx, key, value);
  }

  // 4. Existing Data Node
  // We have a match that we can update, or remove.
  if (isEqual(key, data[dataidx])) {
    return copyAndSet(node, generation, dataidx + 1, value);
  }

  // 5. Collision
  // There is no child node, but a data node with the same hash, but with a different key.
  // To resolve this, we push both nodes down one level.
  const childShift = shift + bits;

  let child = emptyNode;
  child = insertIntoNode(transient, child, key, value, hash, childShift);

  const key2 = data[dataidx];
  const value2 = data[dataidx + 1];
  const hash2 = getHash(key2);
  child = insertIntoNode(transient, child, key2, value2, hash2, childShift);

  // we inserted 2 elements, but implicitely deleted the one we pushed down from the datamap.
  transient.size -= 1;

  // remove the old data pair, and insert the new child node.
  const length = data.length;
  const nodeidx = length - 1 - index(node.nodemap, bit);

  // writing these loops in javascript instead of a combination of splices
  // turns out to be faster. Copying always turned out to be faster.
  const newData = new Array(length - 1);

  let readIndex = 0;
  let writeIndex = 0;

  // [0..dataidx, skip 2 elements, ..nodeidx, newChild, ..rest]
  while (readIndex < dataidx) newData[writeIndex++] = data[readIndex++];
  readIndex += 2;
  while (readIndex <= nodeidx) newData[writeIndex++] = data[readIndex++];
  newData[writeIndex++] = child;
  while (readIndex < length) newData[writeIndex++] = data[readIndex++];

  return makeNode(generation, node.datamap ^ bit, node.nodemap | bit, newData);
}

/**
 * Consume a transient, removing a key if it exists.
 * Returns a new transient.
 */
export function destructiveTransientDelete(key, transient) {
  const hash = getHash(key);
  transient.root = deleteFromNode(transient, transient.root, key, hash, 0);
  return transient;
}

function deleteFromNode(transient, node, key, hash, shift) {
  const data = node.data;
  const generation = transient.generation;

  // 1. Overflow Node
  // overflow nodes only contain key/value-pairs. we walk the data linearly trying to find a match.
  if (shift > 32) {
    for (let i = 0; i < data.length; i += 2) {
      if (isEqual(key, data[i])) {
        transient.size -= 1;
        return copyAndRemovePair(node, generation, 0, i);
      }
    }

    return node;
  }

  const bit = hashbit(hash, shift);
  const dataidx = Math.imul(index(node.datamap, bit), 2);

  // 2. Child Node
  // We have to check first if there is already a child node we have to traverse to.
  if ((node.nodemap & bit) !== 0) {
    const nodeidx = data.length - 1 - index(node.nodemap, bit);

    let child = data[nodeidx];
    child = deleteFromNode(transient, child, key, hash, shift + bits);

    // the node did change, so let's copy to incorporate that change.
    if (child.nodemap !== 0 || child.data.length > 2) {
      return copyAndSet(node, generation, nodeidx, child);
    }

    // this node only has a single data (k/v-pair) child.
    // to restore the CHAMP invariant, we "pull" that pair up into ourselves.
    // this ensures that every tree stays in its single optimal representation,
    // and allows dicts to be structurally compared.
    const length = data.length;
    const newData = new Array(length + 1);

    let readIndex = 0;
    let writeIndex = 0;

    while (readIndex < dataidx) newData[writeIndex++] = data[readIndex++];
    newData[writeIndex++] = child.data[0];
    newData[writeIndex++] = child.data[1];
    while (readIndex < nodeidx) newData[writeIndex++] = data[readIndex++];
    readIndex++;
    while (readIndex < length) newData[writeIndex++] = data[readIndex++];

    return makeNode(generation, node.datamap | bit, node.nodemap ^ bit, newData);
  }

  // 3. Data Node
  // There is no data entry here, or it is a prefix for a different key
  if ((node.datamap & bit) === 0 || !isEqual(key, data[dataidx])) {
    return node;
  }

  // we found a data entry that we can delete.
  transient.size -= 1;
  return copyAndRemovePair(node, generation, bit, dataidx);
}

export function map(dict, fun) {
  // map can never modify the structure, so we can walk the dictionary directly,
  // but still move to a new generation to make sure we get a new copy of every node.
  const generation = nextGeneration(dict);
  const root = copyNode(dict.root, generation);
  const queue = [root];

  while (queue.length) {
    // order doesn't matter, so we can use push/pop for faster array usage.
    const node = queue.pop();
    const data = node.data;
    // every node contains popcount(datamap) direct entries
    // We need to query the length from the nodemap, as we don't know if this
    //  is an overflow node or not! if it is, it will never have datamap set!
    const edgesStart = data.length - popcount(node.nodemap);
    for (let i = 0; i < edgesStart; i += 2) {
      // we copied the node while queueing it, so direct mutation here is safe.
      data[i + 1] = fun(data[i], data[i + 1]);
    }
    // the remaining entries are other nodes we can queue
    for (let i = edgesStart; i < data.length; ++i) {
      // copy the node first to make it safe to mutate
      data[i] = copyNode(data[i], generation);
      queue.push(data[i]);
    }
  }

  return new Dict(dict.size, root);
}

export function fold(dict, state, fun) {
  const queue = [dict.root];

  while (queue.length) {
    // order doesn't matter, so we can use push/pop for faster array usage.
    const node = queue.pop();
    const data = node.data;
    // every node contains popcount(datamap) direct entries
    // We need to query the length from the nodemap, as we don't know if this
    //  is an overflow node or not! if it is, it will never have datamap set!
    const edgesStart = data.length - popcount(node.nodemap);
    for (let i = 0; i < edgesStart; i += 2) {
      state = fun(state, data[i], data[i + 1]);
    }
    // the remaining entries are child nodes we can queue.
    for (let i = edgesStart; i < data.length; ++i) {
      queue.push(data[i]);
    }
  }

  return state;
}

/**
 * How many `1` bits are set in a 32-bit integer.
 */
function popcount(n) {
  n -= (n >>> 1) & 0x55555555;
  n = (n & 0x33333333) + ((n >>> 2) & 0x33333333);
  return Math.imul((n + (n >>> 4)) & 0x0f0f0f0f, 0x01010101) >>> 24;
}

/**
 * Given a population bitmap and a bit selected from that map, returns
 * how many less significant 1 bits there are.
 *
 * For example, index(10101, 100) returns 1, since there is a single less
 * significant `1` bit. This translates to the 0-based "index" of that bit.
 */
function index(bitmap, bit) {
  return popcount(bitmap & (bit - 1));
}

/**
 * Extracts a single slice of the hash, and returns a bitmask for the resulting value.
 * For example, if the slice returns 5, this function returns 10000 = 1 << 5.
 */
function hashbit(hash, shift) {
  return 1 << ((hash >>> shift) & mask);
}

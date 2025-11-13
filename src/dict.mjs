/**
 * This file uses jsdoc to annotate types.
 * These types can be checked using the typescript compiler with "checkjs" option.
 */

import { isEqual, Result$Error, Result$Ok } from "./gleam.mjs";
import { Option$Some, Option$None } from "./gleam/option.mjs";

// -- HASH --------------------------------------------------------------------

const referenceMap = /* @__PURE__ */ new WeakMap();
const tempDataView = /* @__PURE__ */ new DataView(/* @__PURE__ */ new ArrayBuffer(8));
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
 * standard string hash popularised by java
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

class Node {
  #generation;

  constructor(generation, datamap, nodemap, data) {
    // The generation value for transient copy tracking.
    this.#generation = generation;
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
    this.datamap = datamap;
    this.nodemap = nodemap;
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
    this.data = data;
  }

  get generation() {
    // hide generation so it's not enumerable - this makes dicts
    // supported by the default equality and hash codes without custom implementations.
    return this.#generation;
  }

  set generation(value) {
    this.#generation = value;
  }
}

/// The power-of-2 branching factor for the dict. For example, a value of `5` indicates a 32-ary tree.
const bits = 5;
const mask = (1 << bits) - 1;

/// This symbol is used internally to avoid constructing results.
const noElementMarker = Symbol();

// Some commonly used constants throughout the code.
const emptyNode = /* @__PURE__ */ new Node(0, 0, 0, []);
const emptyDict = /* @__PURE__ */ new Dict(0, emptyNode);
const errorNil = /* @__PURE__ */ Result$Error(undefined);

/**
 * Copies a node and its data array if it's from another generation, making it safe
 * to mutate the node.
 */
function copyNode(node, generation) {
  if (node.generation === generation) {
    return node;
  }

  const { datamap, nodemap, data } = node;
  return new Node(generation, datamap, nodemap, data.slice(0));
}

export function make() {
  return emptyDict;
}

export function from(iterable) {
  let transient = toTransient(emptyDict);
  for (const [key, value] of iterable) {
    transient = put(key, value, transient);
  }
  return fromTransient(transient);
}

export function size(dict) {
  return dict.size;
}

export function get(dict, key) {
  const result = lookup(dict.root, key);
  return result !== noElementMarker ? Result$Ok(result) : errorNil;
}

export function has(dict, key) {
  return lookup(dict.root, key) !== noElementMarker;
}

function lookup(node, key) {
  const hash = getHash(key);

  for (let shift = 0; shift < 32; shift += bits) {
    const { data, datamap, nodemap } = node;
    const bit = hashbit(hash, shift);

    if (datamap & bit) {
      // we store this hash directly!
      //
      // this also means that there are no other values with the same
      // hash prefix in this dict.
      //
      // We still need to check if the key matches, but if it does we know for
      // sure that this is the correct value, and if it doesn't that we don't
      // contain the value in question.
      const dataidx = Math.imul(index(datamap, bit), 2);
      return isEqual(key, data[dataidx]) ? data[dataidx + 1] : noElementMarker;
    } else if (nodemap & bit) {
      // we found our hash inside the nodemap, so we can continue our search there.
      node = data[data.length - 1 - index(nodemap, bit)];
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
  if (root.generation < Number.MAX_SAFE_INTEGER) {
    return root.generation + 1;
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
    node.generation = 0;

    // queue all other referenced nodes
    const nodeStart = Math.imul(popcount(node.datamap), 2);
    for (let i = nodeStart; i < node.data.length; ++i) {
      queue.push(node.data[i]);
    }
  }

  return 1;
}

/**
 * Consume a transient, writing a new key/value pair into the dictionary it
 * represents. If the key already exists, it will be overwritten.
 *
 * Returns a new transient.
 */
export function put(key, value, transient) {
  const fun = always(value);
  const hash = getHash(key);
  transient.root = doUpsert(transient, transient.root, key, fun, hash, 0);
  return transient;
}

/**
 * Consume a transient, removing a key if it exists.
 * Returns a new transient.
 */
export function remove(key, transient) {
  return put(key, noElementMarker, transient);
}

export function upsert(dict, key, fun) {
  // we can use our noElementMarker value to skip traversing the dictionary twice.
  const transient = toTransient(dict);
  const wrapped = (value) =>
    fun(value === noElementMarker ? Option$None() : Option$Some(value));
  const hash = getHash(key);
  transient.root = doUpsert(transient, transient.root, key, wrapped, hash, 0);
  return fromTransient(transient);
}

export function update_with(key, fun, init, transient) {
  const wrapped = (value) => (value === noElementMarker ? init : fun(value));
  const hash = getHash(key);
  transient.root = doUpsert(transient, transient.root, key, wrapped, hash, 0);
  return transient;
}

export function map(dict, fun) {
  // map can never modify the structure, so we can walk the dictionary directly,
  // but still move to a new generation to make sure we get a new copy of every node.
  const generation = nextGeneration(dict);
  const root = copyNode(dict.root, generation);
  const queue = [root];

  while (queue.length) {
    // order doesn't matter, so we can use push/pop for faster array usage.
    const { data, datamap } = queue.pop();
    // every node contains popcount(datamap) direct entries
    const edgesStart = Math.imul(popcount(datamap), 2);
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
    const { data, datamap } = queue.pop();
    // every node contains popcount(datamap) direct entries
    const edgesStart = Math.imul(popcount(datamap), 2);
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
 * Main helper function for insert/upsert/remove.
 */
function doUpsert(transient, node, key, fun, hash, shift) {
  const { data, datamap, nodemap } = node;

  // 1. Overflow Node
  // overflow nodes only contain key/value-pairs. we walk the data linearly trying to find a match.
  if (shift > 32) {
    for (let i = 0; i < data.length; i += 2) {
      if (isEqual(key, data[i])) {
        return doUpdate(transient, node, fun, 0, i);
      }
    }

    return doInsert(transient, node, key, fun, 0, data.length);
  }

  const bit = hashbit(hash, shift);
  const nodeidx = data.length - 1 - index(nodemap, bit);
  const dataidx = Math.imul(index(datamap, bit), 2);

  // 2. Child Node
  // We have to check first if there is already a child node we have to traverse to.
  if ((nodemap & bit) !== 0) {
    const oldChild = data[nodeidx];
    const newChild = doUpsert(transient, oldChild, key, fun, hash, shift + bits);
    if (newChild === oldChild) {
      return node;
    }

    // the node did change, so let's copy to incorporate that change.
    node = copyNode(node, transient.generation);
    if (newChild.nodemap !== 0 || newChild.data.length > 2) {
      node.data[nodeidx] = newChild;
    } else {
      // this node only has a single data (k/v-pair) child.
      // to restore the CHAMP invariant, we "pull" that pair up into ourselves.
      // this ensures that every tree stays in its single optimal representation,
      // and allows dicts to be structurally compared.
      node.datamap |= bit;
      node.nodemap ^= bit;
      // NOTE: the order here is important to avoid mutation bugs!
      // Remove the old child node, and insert the data pair into ourselves.
      node.data.splice(nodeidx, 1);
      node.data.splice(dataidx, 0, newChild.data[0], newChild.data[1]);
    }

    return node;
  }

  // 3. New Data Node
  // No child node and no data node exists yet, so we can potentially just insert a new value.
  if ((datamap & bit) === 0) {
    return doInsert(transient, node, key, fun, bit, dataidx);
  }

  // 4. Existing Data Node
  // We have a match that we can update, or remove.
  if (isEqual(key, data[dataidx])) {
    return doUpdate(transient, node, fun, bit, dataidx);
  }

  // 5. Collision
  // There is no child node, but a data node with the same hash, but with a different key.
  // To resolve this, we push both nodes down one level.
  let child = new Node(transient.generation, 0, 0, []);
  child = doUpsert(transient, child, key, fun, hash, shift + bits);
  if (!child.data.length) {
    return node;
  }

  const otherKey = data[dataidx];
  const childHash = getHash(otherKey);
  const childFun = always(data[dataidx + 1]);
  child = doUpsert(transient, child, otherKey, childFun, childHash, shift + bits);
  // we inserted 2 elements, but implicitely deleted the one we pushed down from the datamap.
  transient.size -= 1;

  node = copyNode(node, transient.generation);
  node.datamap ^= bit;
  node.nodemap |= bit;

  // remove the old data pair, and insert the new child node.
  // because we remove 2 elements first, our indices are off-by-one!
  // When calculating the nodeidx, we measure with the length including those
  // 2 extra elements, but missing the one we haven't inserted yet, so we have
  // to correct for both of these with (1-2) = -1
  node.data.splice(dataidx, 2);
  node.data.splice(nodeidx - 1, 0, child);

  return node;
}

function doUpdate(transient, node, fun, bit, index) {
  node = copyNode(node, transient.generation);

  const value = fun(node.data[index + 1]);

  if (value === noElementMarker) {
    node.data.splice(index, 2);
    node.datamap ^= bit;
    transient.size -= 1;
  } else {
    node.data[index + 1] = value;
  }

  return node;
}

function doInsert(transient, node, key, fun, bit, index) {
  const value = fun(noElementMarker);
  if (value === noElementMarker) {
    return node;
  }

  node = copyNode(node, transient.generation);

  node.datamap |= bit;
  node.data.splice(index, 0, key, value);
  transient.size += 1;

  return node;
}

function always(value) {
  return (_) => value;
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
 * Extracts a single slice ofthe hash, and returns a bitmask for the resulting value.
 * For example, if the slice returns 5, this function returns 10000 = 1 << 5.
 */
function hashbit(hash, shift) {
  return 1 << ((hash >>> shift) & mask);
}

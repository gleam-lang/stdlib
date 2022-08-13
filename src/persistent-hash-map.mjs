import { isEqual } from "./gleam.mjs";

const referenceMap = new WeakMap();
let referenceUID = 0;

/** hash the object by reference using a weak map and incrementing uid */
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
/** merge two hashes in an order sensitive way */
function hashMerge(a, b) {
  return (a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2))) | 0;
}
/** standard string hash popularised by java */
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = (Math.imul(31, hash) + s.charCodeAt(i)) | 0;
  }
  return hash;
}
/** convert number to string and hash, seems to be better and faster than anything else */
function hashNumber(n) {
  return hashString(n.toString());
}
/** hash any js object */
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      return o.hashCode(o);
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
/** hash any js value */
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
      return hashNumber(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
  }
}
const SHIFT = 5; // number of bits you need to shift by to get the next bucket
const BUCKET_SIZE = Math.pow(2, SHIFT);
const MASK = BUCKET_SIZE - 1; // used to zero out all bits not in the bucket
const MAX_INDEX_NODE = BUCKET_SIZE / 2; // when does index node grow into array node
const MIN_ARRAY_NODE = BUCKET_SIZE / 4; // when does array node shrink to index node
const ENTRY = 0;
const ARRAY_NODE = 1;
const INDEX_NODE = 2;
const COLLISION_NODE = 3;
const EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: [],
};
/** Mask the hash to get only the bucket corresponding to shift */
function mask(hash, shift) {
  return (hash >>> shift) & MASK;
}
/** Set only the Nth bit where N is the masked hash */
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
/** Count the number of 1 bits in a number */
function bitcount(x) {
  x -= (x >> 1) & 0x55555555;
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x += x >> 8;
  x += x >> 16;
  return x & 0x7f;
}
/** Calculate the array index of an item in a bitmap index node */
function index(bitmap, bit) {
  return bitcount(bitmap & (bit - 1));
}
/** Efficiently copy an array and set one value at an index */
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
/** Efficiently copy an array and insert one value at an index */
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
/** Efficiently copy an array and remove one value at an index */
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
/** Create a new node containing two entries */
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 },
      ],
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
/** Associate a node with a new entry, creating a new node. */
function assoc(root, shift, hash, key, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  // if the corresponding index is empty set the index to a newly created node
  if (node === undefined) {
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(
        root.array,
        idx,
        assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf)
      ),
    };
  }
  // otherwise call assoc on the child node
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  // if the child node hasn't changed just return the old root
  if (n === node) {
    return root;
  }
  // otherwise set the index to the new node
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n),
  };
}
function assocIndex(root, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  // if there is already a item at this hash index..
  if ((root.bitmap & bit) !== 0) {
    // if there is a node at the index (not an entry), call assoc on the child node
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n),
      };
    }
    // otherwise there is an entry at the index
    // if the keys are equal replace the entry with the updated value
    const keyOrNull = node.k;
    if (isEqual(key, keyOrNull)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: keyOrNull,
          v: val,
        }),
      };
    }
    // if the keys are not equal, replace the entry with a new child node
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, keyOrNull, node.v, hash, key, val)
      ),
    };
  } else {
    // else there is currently no item at the hash index
    const n = root.array.length;
    // if the number of nodes is at the maximum, expand this node into an array node
    if (n >= MAX_INDEX_NODE) {
      // create a 32 length array for the new array node (one for each bit in the hash)
      const nodes = new Array(32);
      // create and insert a node for the new entry
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      // place each item in the index node into the correct spot in the array node
      // loop through all 32 bits / array positions
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          // turn any entries into index nodes
          // since array nodes should only contain other nodes, not entries
          if (node.type !== ENTRY) {
            nodes[i] = node;
          } else {
            nodes[i] = assocIndex(
              EMPTY,
              shift + SHIFT,
              getHash(node.k),
              node.k,
              node.v,
              addedLeaf
            );
          }
        }
        // shift the bitmap to process the next bit
        bitmap >>>= 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes,
      };
    } else {
      // else there is still space in this index node
      // simply insert a new entry at the hash index
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val,
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray,
      };
    }
  }
}
function assocCollision(root, shift, hash, key, val, addedLeaf) {
  // if there is a hash collision
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key);
    // if this key already exists replace the entry with the new value
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash: hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val }),
      };
    }
    // otherwise insert the entry at the end of the array
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash: hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key, v: val }),
    };
  }
  // if there is no hash collision, upgrade to an index node
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root.hash, shift),
      array: [root],
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
/** Find the index of a key in the collision node's array */
function collisionIndexOf(root, key) {
  const size = root.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
/** Return the found entry or undefined if not present in the root */
function find(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root, key);
  }
}
function findArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === undefined) {
    return undefined;
  }
  return find(node, shift + SHIFT, hash, key);
}
function findIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return undefined;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return undefined;
}
function findCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return undefined;
  }
  return root.array[idx];
}
/**
 * Remove an entry from the root, returning the updated root.
 * Returns undefined if the node should be removed from the parent.
 * */
function without(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root, key);
  }
}
function withoutArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === undefined) {
    return root; // already empty
  }
  const n = without(node, shift + SHIFT, hash, key);
  if (n === node) {
    return root; // no changes
  }
  // if the recursive call returned undefined the node should be removed
  if (n === undefined) {
    // if the number of child nodes is at the minimum, pack into an index node
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i; // skip copying the removed node
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap: bitmap,
        array: out,
      };
    }
    return {
      type: ARRAY_NODE,
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n),
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n),
  };
}
function withoutIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root; // already empty
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  // if the item is not an entry
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root; // no changes
    }
    // if not undefined, the child node still has items, so update it
    if (n !== undefined) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n),
      };
    }
    // otherwise the child node should be removed
    // if it was the only child node, remove this node from the parent
    if (root.bitmap === bit) {
      return undefined;
    }
    // otherwise just remove the child node
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx),
    };
  }
  // otherwise the item is an entry, remove it if the key matches
  if (isEqual(key, node.k)) {
    if (root.bitmap === bit) {
      return undefined;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx),
    };
  }
  return root;
}
function withoutCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  // if the key not found, no changes
  if (idx < 0) {
    return root;
  }
  // otherwise the entry was found, remove it
  // if it was the only entry in this node, remove the whole node
  if (root.array.length === 1) {
    return undefined;
  }
  // otherwise just remove the entry
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx),
  };
}
function forEach(root, fn) {
  if (root === undefined) {
    return;
  }
  const items = root.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item = items[i];
    if (item === undefined) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
/** Extra wrapper to keep track of map size */
export class PMap {
  constructor(root, size) {
    this.root = root;
    this.size = size;
  }
  hashCode() {
    let h = 0;
    forEach(this.root, (v, k) => {
      h = (h + hashMerge(getHash(v), getHash(k))) | 0;
    });
    return h;
  }
  equals(o) {
    let equal = true;
    forEach(this.root, (v, k) => {
      equal = equal && isEqual(getWithDefault(o, k, !v), v);
    });
    return equal;
  }
}
export function create() {
  return new PMap(undefined, 0);
}
export function getWithDefault(map, key, notFound) {
  if (map.root === undefined) {
    return notFound;
  }
  const found = find(map.root, 0, getHash(key), key);
  if (found === undefined) {
    return notFound;
  }
  return found.v;
}
export function set(map, key, val) {
  const addedLeaf = { val: false };
  const root = map.root === undefined ? EMPTY : map.root;
  const newRoot = assoc(root, 0, getHash(key), key, val, addedLeaf);
  if (newRoot === map.root) {
    return map;
  }
  return new PMap(newRoot, addedLeaf.val ? map.size + 1 : map.size);
}
export function remove(map, key) {
  if (map.root === undefined) {
    return map;
  }
  const newRoot = without(map.root, 0, getHash(key), key);
  if (newRoot === map.root) {
    return map;
  }
  if (newRoot === undefined) {
    return create();
  }
  return new PMap(newRoot, map.size - 1);
}
export function has(map, key) {
  if (map.root === undefined) {
    return false;
  }
  return find(map.root, 0, getHash(key), key) !== undefined;
}
export function entries(map) {
  if (map.root === undefined) {
    return [];
  }
  const result = [];
  forEach(map.root, (v, k) => result.push([k, v]));
  return result;
}
export function __include_me() {}

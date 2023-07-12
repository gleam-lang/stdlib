import fc from "https://cdn.skypack.dev/fast-check";
import PMap from "../build/dev/javascript/gleam_stdlib/persistent-hash-map.mjs";

const options = Object.freeze({
  numRuns: 1000_000,
});

Deno.test("all inserted numbers must exist within", () => {
  const property = (numbers: Array<number>) => {
    let map = PMap.new();

    // Insert all numbers
    for (const number of numbers) {
      map = map.set(number, 1);
    }

    // Check that all numbers are in the map
    for (const number of numbers) {
      if (!map.has(number)) {
        console.log(`Number ${number} not found`);
        return false;
      }
    }
  };

  const arbitrary = fc.array(fc.integer());
  fc.assert(fc.property(arbitrary, property), options);
});

Deno.test("all inserted values can be got", () => {
  const property = (numbers: Array<[number, number]>) => {
    const reference = new Map();
    let map = PMap.new();

    // Insert all pairs
    for (const [k, v] of numbers) {
      reference.set(k, v);
      map = map.set(k, v);
    }

    // Check that all keys have the correct value
    for (const [k, _] of numbers) {
      const expected = reference.get(k);
      const found = map.get(k, undefined);
      if (found !== expected) {
        console.log(`${k} was ${found} not ${expected}`);
        return false;
      }
    }
  };

  const arbitrary = fc.array(fc.tuple(fc.integer(), fc.integer()));
  fc.assert(fc.property(arbitrary, property), options);
});

Deno.test("size", () => {
  const property = (numbers: Array<number>) => {
    const reference = new Map();
    let map = PMap.new();

    // Insert all values
    for (const k of numbers) {
      reference.set(k, 1);
      map = map.set(k, 1);
    }

    // Map size should match reference
    if (map.size !== reference.size) {
      console.log(`size was ${map.size} not ${reference.size}`);
      return false;
    }
  };

  const arbitrary = fc.array(fc.integer());
  fc.assert(fc.property(arbitrary, property), options);
});

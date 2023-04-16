import { assertObjectMatch } from "https://deno.land/std@0.183.0/testing/asserts.ts";
import { decode_map } from "../build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs";

Deno.test("does not throw when decoding null", () => {
  const result = decode_map(null);

  assertObjectMatch(result, {
    0: {
      head: { expected: "Map", found: "Atom", path: {} },
      tail: {},
    },
  });
});

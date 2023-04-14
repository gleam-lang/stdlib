import { assertObjectMatch } from "https://deno.land/std@0.183.0/testing/asserts.ts";
import { decode_map } from "../build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs";

Deno.test("does not throw when decoding null", () => {
  const result = decode_map(null);

  assertObjectMatch(result, {
    0: {
      head: { expected: "Map", found: "Null", path: {} },
      tail: {},
    },
  });
});

Deno.test("does not throw when decoding undefined", () => {
  const result = decode_map(undefined);

  assertObjectMatch(result, {
    0: {
      head: { expected: "Map", found: "Nil", path: {} },
      tail: {},
    },
  });
});

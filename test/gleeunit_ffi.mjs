import { readFileSync } from "node:fs";
import { Ok, Error as GleamError } from "./gleam.mjs";
import * as reporting from "./gleeunit/internal/reporting.mjs";

export function read_file(path) {
  try {
    return new Ok(readFileSync(path));
  } catch {
    return new GleamError(undefined);
  }
}

async function* gleamFiles(directory) {
  for (let entry of await read_dir(directory)) {
    let path = join_path(directory, entry);
    if (path.endsWith(".gleam")) {
      yield path;
    } else {
      try {
        yield* gleamFiles(path);
      } catch (error) {
        // Could not read directory, assume it's a file
      }
    }
  }
}

async function readRootPackageName() {
  let toml = await async_read_file("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main() {
  let state = reporting.new_state();

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  for await (let path of await gleamFiles("test")) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    let module = await import(join_path(dist, js_path));
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) continue;
      try {
        await module[fnName]();
        state = reporting.test_passed(state);
      } catch (error) {
        let moduleName = js_path.slice(0, -4);
        state = reporting.test_failed(state, moduleName, fnName, error);
      }
    }
  }

  const status = reporting.finished(state);
  exit(status);
}

export function crash(message) {
  throw new Error(message);
}

function exit(code) {
  if (globalThis.Deno) {
    Deno.exit(code);
  } else {
    process.exit(code);
  }
}

async function read_dir(path) {
  if (globalThis.Deno) {
    let items = [];
    for await (let item of Deno.readDir(path, { withFileTypes: true })) {
      items.push(item.name);
    }
    return items;
  } else {
    let { readdir } = await import("node:fs/promises");
    return readdir(path);
  }
}

function join_path(a, b) {
  if (a.endsWith("/")) return a + b;
  return a + "/" + b;
}

async function async_read_file(path) {
  if (globalThis.Deno) {
    return Deno.readTextFile(path);
  } else {
    let { readFile } = await import("node:fs/promises");
    let contents = await readFile(path);
    return contents.toString();
  }
}

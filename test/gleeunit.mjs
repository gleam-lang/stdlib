// This file is a verbatim copy of gleeunit 0.10.0's <https://github.com/lpil/gleeunit/blob/main/src/gleeunit_ffi.mjs>

async function* gleamFiles(directory) {
  for (const entry of await read_dir(directory)) {
    const path = join_path(directory, entry);
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
  const toml = await read_file("gleam.toml", "utf-8");
  for (const line of toml.split("\n")) {
    const matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

export async function main() {
  let passes = 0;
  let failures = 0;

  const packageName = await readRootPackageName();
  const dist = `../${packageName}/`;

  for await (const path of await gleamFiles("test")) {
    const js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    const module = await import(join_path(dist, js_path));
    for (const fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) continue;
      try {
        await module[fnName]();
        write(`\u001b[32m.\u001b[0m`);
        passes++;
      } catch (error) {
        const moduleName = "\n" + js_path.slice(0, -4);
        const line = error.line ? `:${error.line}` : "";
        write(`\n❌ ${moduleName}.${fnName}${line}: ${error}\n`);
        failures++;
      }
    }
  }

  console.log(`
${passes + failures} tests, ${failures} failures`);
  exit(failures ? 1 : 0);
}

export function crash(message) {
  throw new Error(message);
}

function write(message) {
  if (globalThis.Deno) {
    Deno.stdout.writeSync(new TextEncoder().encode(message));
  } else {
    process.stdout.write(message);
  }
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
    const items = [];
    for await (const item of Deno.readDir(path, { withFileTypes: true })) {
      items.push(item.name);
    }
    return items;
  } else {
    let { readdir } = await import("fs/promises");
    return readdir(path);
  }
}

function join_path(a, b) {
  if (a.endsWith("/")) return a + b;
  return a + "/" + b;
}

async function read_file(path) {
  if (globalThis.Deno) {
    return Deno.readTextFile(path);
  } else {
    const { readFile } = await import("fs/promises");
    const contents = await readFile(path);
    return contents.toString();
  }
}

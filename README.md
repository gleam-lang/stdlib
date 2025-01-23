# stdlib

[![Package Version](https://img.shields.io/hexpm/v/storail)](https://hex.pm/packages/storail)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/storail/)
[![Discord chat](https://img.shields.io/discord/768594524158427167?color=blue)](https://discord.gg/Fm8Pwmy)

Gleam's standard library!
Documentation available on [HexDocs](https://hexdocs.pm/gleam_stdlib/).

## Installation

Add `gleam_stdlib` to your Gleam project.

```sh
gleam add gleam_stdlib
```

## Usage

Import the modules you want to use and write some code!

```gleam
import gleam/string

pub fn greet(name: String) -> String {
  string.concat(["Hello ", name, "!"])
}
```

## Targets

Gleam's standard library supports both targets: Erlang and JavaScript.

### Compatibility

This library is compatible with all versions of Erlang/OTP 26 and higher, 
as well as all NodeJS, Deno, Bun, and major browsers that are currently
supported by their maintainers. If you have a compatibility issue with
any platform open an issue and we'll see what we can do to help.

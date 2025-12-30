# stdlib

[![Package Version](https://img.shields.io/hexpm/v/gleam_stdlib)](https://hex.pm/packages/gleam_stdlib)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_stdlib/)
[![Discord chat](https://img.shields.io/discord/768594524158427167?color=blue)](https://discord.gg/Fm8Pwmy)

Gleam's standard library!
Documentation available on [HexDocs](https://hexdocs.pm/gleam_stdlib/).

## Installation

Add `gleam_stdlib` to your Gleam project.

```sh
gleam add gleam_stdlib
```
```gleam
import gleam/io

pub fn greet(name: String) -> Nil {
  io.println("Hello " <> name <> "!")
}
```

## Targets

Gleam's standard library supports both targets: Erlang and JavaScript.

### Compatibility

This library is compatible with all versions of Erlang/OTP 26 and higher, 
as well as all NodeJS, Deno, Bun, and major browsers that are currently
supported by their maintainers. If you have a compatibility issue with
any platform open an issue and we'll see what we can do to help.

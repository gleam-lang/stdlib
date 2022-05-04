# stdlib

<a href="https://github.com/gleam-lang/stdlib/releases"><img src="https://img.shields.io/github/release/gleam-lang/stdlib" alt="GitHub release"></a>
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
![CI](https://github.com/gleam-lang/stdlib/workflows/CI/badge.svg?branch=main)

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
import gleam/list.{contains}

fn usage() {
  string.append("str", "ing")
}

fn more_usage() {
  [1, 2, 3]
  |> contains(any: 2)
}
```

## Targets

Gleam's standard library supports both targets: Erlang and JavaScript.

### Compatibility

This library is compatible with all versions of Erlang/OTP, NodeJS, and
major browsers that are currently supported by their maintainers. If you
have a compatibility issue with any platform open an issue and we'll see
what we can do to help.

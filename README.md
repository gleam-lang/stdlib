# stdlib

<a href="https://github.com/gleam-lang/stdlib/releases"><img src="https://img.shields.io/github/release/gleam-lang/stdlib" alt="GitHub release"></a>
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
![CI](https://github.com/gleam-lang/stdlib/workflows/CI/badge.svg?branch=main)

Gleam's standard library!
Documentation available on [HexDocs](https://hexdocs.pm/gleam_stdlib/).

## Installation

Add `gleam_stdlib` to the deps section of your `rebar.config` or `mix.exs`

```erlang
{deps, [
    {gleam_stdlib, "~> 0.15.0"}
]}
```

```elixir
defp deps do
  [
    {:gleam_stdlib, "~> 0.15.0"},
  ]
end
```

## Usage

Import the modules you want to use and write some code!

```rust
import gleam/string
import gleam/list.{contains}

fn usage() {
  string.append("str", "ing")
}

fn more_usage() {
  contains([1, 2, 3], any: 2)
}
```

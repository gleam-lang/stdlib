//// Use the `assert` keyword instead of this module.

import gleam/option.{type Option, None, Some}
import gleam/string

pub fn equal(a: t, b: t) -> Nil {
  case a == b {
    True -> Nil
    _ ->
      panic as string.concat([
        "\n",
        string.inspect(a),
        "\nshould equal\n",
        string.inspect(b),
      ])
  }
}

pub fn not_equal(a: t, b: t) -> Nil {
  case a != b {
    True -> Nil
    _ ->
      panic as string.concat([
        "\n",
        string.inspect(a),
        "\nshould not equal\n",
        string.inspect(b),
      ])
  }
}

pub fn be_ok(a: Result(a, e)) -> a {
  case a {
    Ok(value) -> value
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be ok"])
  }
}

pub fn be_error(a: Result(a, e)) -> e {
  case a {
    Error(error) -> error
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be error"])
  }
}

pub fn be_some(a: Option(a)) -> a {
  case a {
    Some(value) -> value
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be some"])
  }
}

pub fn be_none(a: Option(a)) -> Nil {
  case a {
    None -> Nil
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be none"])
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

pub fn fail() -> Nil {
  be_true(False)
}

import gleam/bit_array
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import gleeunit/internal/gleam_panic.{type GleamPanic}

pub type State {
  State(passed: Int, failed: Int, skipped: Int)
}

pub fn new_state() -> State {
  State(passed: 0, failed: 0, skipped: 0)
}

pub fn finished(state: State) -> Int {
  case state {
    State(passed: 0, failed: 0, skipped: 0) -> {
      io.println("\nNo tests found!")
      1
    }
    State(failed: 0, skipped: 0, ..) -> {
      let message = "\n" <> int.to_string(state.passed) <> " tests, no failures"
      io.println(green(message))
      0
    }
    State(skipped: 0, ..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, "
        <> int.to_string(state.failed)
        <> " failures"
      io.println(red(message))
      0
    }
    State(failed: 0, ..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, 0 failures, "
        <> int.to_string(state.skipped)
        <> " skipped"
      io.println(yellow(message))
      1
    }
    State(..) -> {
      let message =
        "\n"
        <> int.to_string(state.passed)
        <> " tests, "
        <> int.to_string(state.failed)
        <> " failures, "
        <> " skipped"
      io.println(red(message))
      1
    }
  }
}

pub fn test_passed(state: State) -> State {
  io.print(green("."))
  State(..state, passed: state.passed + 1)
}

pub fn test_failed(
  state: State,
  module: String,
  function: String,
  error: dynamic.Dynamic,
) -> State {
  let message = case gleam_panic.from_dynamic(error) {
    Ok(error) -> {
      let src = option.from_result(read_file(error.file))
      format_gleam_error(error, module, function, src)
    }
    Error(_) -> format_unknown(module, function, error)
  }

  io.print("\n" <> message)
  State(..state, failed: state.failed + 1)
}

fn format_unknown(
  module: String,
  function: String,
  error: dynamic.Dynamic,
) -> String {
  string.concat([
    grey(module <> "." <> function) <> "\n",
    "An unexpected error occurred:\n",
    "\n",
    "  " <> string.inspect(error) <> "\n",
  ])
}

fn format_gleam_error(
  error: GleamPanic,
  module: String,
  function: String,
  src: Option(BitArray),
) -> String {
  let location = grey(error.file <> ":" <> int.to_string(error.line))

  case error.kind {
    gleam_panic.Panic -> {
      string.concat([
        bold(red("panic")) <> " " <> location <> "\n",
        cyan(" test") <> ": " <> module <> "." <> function <> "\n",
        cyan(" info") <> ": " <> error.message <> "\n",
      ])
    }

    gleam_panic.Todo -> {
      string.concat([
        bold(yellow("todo")) <> " " <> location <> "\n",
        cyan(" test") <> ": " <> module <> "." <> function <> "\n",
        cyan(" info") <> ": " <> error.message <> "\n",
      ])
    }

    gleam_panic.Assert(start:, end:, kind:, ..) -> {
      string.concat([
        bold(red("assert")) <> " " <> location <> "\n",
        cyan(" test") <> ": " <> module <> "." <> function <> "\n",
        code_snippet(src, start, end),
        assert_info(kind),
        cyan(" info") <> ": " <> error.message <> "\n",
      ])
    }

    gleam_panic.LetAssert(start:, end:, value:, ..) -> {
      string.concat([
        bold(red("let assert")) <> " " <> location <> "\n",
        cyan(" test") <> ": " <> module <> "." <> function <> "\n",
        code_snippet(src, start, end),
        cyan("value") <> ": " <> string.inspect(value) <> "\n",
        cyan(" info") <> ": " <> error.message <> "\n",
      ])
    }
  }
}

fn assert_info(kind: gleam_panic.AssertKind) -> String {
  case kind {
    gleam_panic.BinaryOperator(left:, right:, ..) -> {
      string.concat([assert_value(" left", left), assert_value("right", right)])
    }

    gleam_panic.FunctionCall(arguments:) -> {
      arguments
      |> list.index_map(fn(e, i) {
        let number = string.pad_start(int.to_string(i), 5, " ")
        assert_value(number, e)
      })
      |> string.concat
    }

    gleam_panic.OtherExpression(..) -> ""
  }
}

fn assert_value(name: String, value: gleam_panic.AssertedExpression) -> String {
  cyan(name) <> ": " <> inspect_value(value) <> "\n"
}

fn inspect_value(value: gleam_panic.AssertedExpression) -> String {
  case value.kind {
    gleam_panic.Unevaluated -> grey("unevaluated")
    gleam_panic.Literal(..) -> grey("literal")
    gleam_panic.Expression(value:) -> string.inspect(value)
  }
}

fn code_snippet(src: Option(BitArray), start: Int, end: Int) -> String {
  {
    use src <- result.try(option.to_result(src, Nil))
    use snippet <- result.try(bit_array.slice(src, start, end - start))
    use snippet <- result.try(bit_array.to_string(snippet))
    let snippet = cyan(" code") <> ": " <> snippet <> "\n"
    Ok(snippet)
  }
  |> result.unwrap("")
}

pub fn test_skipped(state: State, module: String, function: String) -> State {
  io.print("\n" <> module <> "." <> function <> yellow(" skipped"))
  State(..state, skipped: state.skipped + 1)
}

fn bold(text: String) -> String {
  "\u{001b}[1m" <> text <> "\u{001b}[22m"
}

fn cyan(text: String) -> String {
  "\u{001b}[36m" <> text <> "\u{001b}[39m"
}

fn yellow(text: String) -> String {
  "\u{001b}[33m" <> text <> "\u{001b}[39m"
}

fn green(text: String) -> String {
  "\u{001b}[32m" <> text <> "\u{001b}[39m"
}

fn red(text: String) -> String {
  "\u{001b}[31m" <> text <> "\u{001b}[39m"
}

fn grey(text: String) -> String {
  "\u{001b}[90m" <> text <> "\u{001b}[39m"
}

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, dynamic.Dynamic) {
  case read_file_text(path) {
    Ok(text) -> Ok(bit_array.from_string(text))
    Error(e) -> Error(e)
  }
}

@external(javascript, "../../gleeunit_ffi.mjs", "read_file")
fn read_file_text(path: String) -> Result(String, dynamic.Dynamic)

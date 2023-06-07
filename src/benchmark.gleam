import gleam/io
import gleam/list
import gleam/string

pub fn main() -> Nil {
  io.print("Running benchmarks...\n")

  run_benchmarks()
}

if erlang {
  import glychee/benchmark

  pub fn run_benchmarks() -> Nil {
    let bench_data =
      list.repeat("The quick brown fox jumps over the lazy dog", 10_000_000)

    benchmark.run(
      [
        benchmark.Function(
          label: "string.join (current impl)",
          callable: fn(bench_data) {
            fn() {
              bench_data
              |> string.join_old("\n")
            }
          },
        ),
        benchmark.Function(
          label: "string.join (gleam impl)",
          callable: fn(bench_data) {
            fn() {
              bench_data
              |> string.join_gleam("\n")
            }
          },
        ),
      ],
      [benchmark.Data(label: "bench_data", data: bench_data)],
    )
  }
}

if javascript {
  import gleam/int

  external type Timer

  pub fn run_benchmarks() -> Nil {
    let bench_data =
      list.repeat("The quick brown fox jumps over the lazy dog", 10_000_000)

    let bench_timer = create_timer()
    bench_data
    |> string.join_old("\n")
    let timing = read_timer(bench_timer)
    io.print(
      "string.join (current_impl) took " <> int.to_string(timing) <> "ms\n",
    )

    let bench_timer = create_timer()
    bench_data
    |> string.join_gleam("\n")
    let timing = read_timer(bench_timer)
    io.print(
      "string.join (gleam impl) took " <> int.to_string(timing) <> "ms\n",
    )

    let bench_timer = create_timer()
    bench_data
    |> string.join("\n")
    let timing = read_timer(bench_timer)
    io.print("string.join (js ffi impl) " <> int.to_string(timing) <> "ms\n")

    Nil
  }

  external fn create_timer() -> Timer =
    "./gleam_stdlib.mjs" "create_timer"

  external fn read_timer(timer: Timer) -> Int =
    "./gleam_stdlib.mjs" "read_timer"
}

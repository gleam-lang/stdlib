import gleam/list
import gleam/int

pub fn main() {
  let sorting_fn_1 = fn() {
    list.range(1, 100_000)
    |> list.reverse
    |> list.sort(int.compare)
  }

  let sorting_fn_2 = fn() {
    list.range(1, 100_000)
    |> list.reverse
    |> list.sort(int.compare)
  }

  run_on_benchee_base(
    #("sorting_fn_1", sorting_fn_1),
    #("sorting_fn_2", sorting_fn_2),
  )
}

external fn run_on_benchee_base(
  #(String, fn() -> List(Int)),
  #(String, fn() -> List(Int)),
) -> a =
  "Elixir.BencheeBase" "run"

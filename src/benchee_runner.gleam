import gleam/list
import gleam/int

pub fn main() {
  let test_data =
    list.range(1, 100_000)
    |> list.reverse

  let sort_fn = fn() {
    test_data
    |> list.sort(int.compare)
  }

  let insertion_sort_tailrec_fn = fn() {
    test_data
    |> list.insertion_sort_tailrec(int.compare)
  }

  run_on_benchee_base(
    #("list.merge_sort()", sort_fn),
    #("list.insertion_sort_tailrec()", insertion_sort_tailrec_fn),
  )
}

external fn run_on_benchee_base(
  #(String, fn() -> List(Int)),
  #(String, fn() -> List(Int)),
) -> a =
  "Elixir.BencheeBase" "run"

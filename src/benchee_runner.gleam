import gleam/list
import gleam/int
import gleam/io

pub fn main() {
  let merge_sort_fn = fn(test_data) {
    fn() {
      test_data
      |> list.merge_sort(int.compare)
    }
  }

  let insertion_sort_fn = fn(test_data) {
    fn() {
      test_data
      |> list.insertion_sort(int.compare)
    }
  }

  let merge_sort_tailrec_fn = fn(test_data) {
    fn() {
      test_data
      |> list.merge_sort_tailrec(int.compare)
    }
  }

  let insertion_sort_tailrec_fn = fn(test_data) {
    fn() {
      test_data
      |> list.insertion_sort_tailrec(int.compare)
    }
  }

  // Bechmarks run for ranges of tiny: 20, medium: 20*50 = 1000 and large: 20*50*50 = 50_000
  // Bechmarks run for reversed lists and for shuffled lists
  //
  io.print("\n\n")
  io.println("===========================================")
  io.println("============ tiny reverse test ============")
  io.println("===========================================")
  io.print("\n")
  let tiny_test_data =
    list.range(1, 20)
    |> list.reverse
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(tiny_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(tiny_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(tiny_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(tiny_test_data),
    ),
  ])

  io.print("\n\n")
  io.println("===========================================")
  io.println("=========== medium reverse test ===========")
  io.println("===========================================")
  io.print("\n")
  let medium_test_data =
    list.range(1, 1_000)
    |> list.reverse
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(medium_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(medium_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(medium_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(medium_test_data),
    ),
  ])

  io.print("\n\n")
  io.println("===========================================")
  io.println("=========== large reverse test ============")
  io.println("===========================================")
  io.print("\n")
  let large_test_data =
    list.range(1, 50_000)
    |> list.reverse
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(large_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(large_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(large_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(large_test_data),
    ),
  ])

  io.print("\n\n")
  io.println("===========================================")
  io.println("============ tiny shuffle test ============")
  io.println("===========================================")
  io.print("\n")
  let tiny_test_data =
    list.range(1, 20)
    |> list_shuffle
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(tiny_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(tiny_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(tiny_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(tiny_test_data),
    ),
  ])

  io.print("\n\n")
  io.println("===========================================")
  io.println("=========== medium shuffle test ===========")
  io.println("===========================================")
  io.print("\n")
  let medium_test_data =
    list.range(1, 1_000)
    |> list_shuffle
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(medium_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(medium_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(medium_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(medium_test_data),
    ),
  ])

  io.print("\n\n")
  io.println("===========================================")
  io.println("=========== large shuffle test ============")
  io.println("===========================================")
  io.print("\n")
  let large_test_data =
    list.range(1, 50_000)
    |> list_shuffle
  run_on_benchee_base([
    #("list.merge_sort()", merge_sort_fn(large_test_data)),
    #("list.insertion_sort()", insertion_sort_fn(large_test_data)),
    #("list.merge_sort_tailrec()", merge_sort_tailrec_fn(large_test_data)),
    #(
      "list.insertion_sort_tailrec()",
      insertion_sort_tailrec_fn(large_test_data),
    ),
  ])
}

external fn run_on_benchee_base(List(#(String, fn() -> List(Int)))) -> a =
  "Elixir.BencheeBase" "run"

external fn list_shuffle(List(a)) -> List(a) =
  "Elixir.Enum" "shuffle"

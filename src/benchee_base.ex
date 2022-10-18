defmodule BencheeBase do
  def run(function_tuple_a, function_tuple_b) do
    Benchee.run(
      %{
        elem(function_tuple_a, 0) => elem(function_tuple_a, 1),
        elem(function_tuple_b, 0) => elem(function_tuple_b, 1)
      },
      time: 10,
      memory_time: 2
    )
  end
end

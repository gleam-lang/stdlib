defmodule BencheeBase do
  def run(list_of_function_tuples) do
    map_of_function_tuples = list_of_function_tuples |> Enum.into(%{})

    Benchee.run(
      map_of_function_tuples,
      time: 10,
      memory_time: 2
    )
  end
end

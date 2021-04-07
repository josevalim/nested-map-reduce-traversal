sections = [
  %{
    "title" => "Getting started",
    "reset_lesson_position" => false,
    "lessons" => [
      %{"name" => "Welcome"},
      %{"name" => "Installation"}
    ]
  },
  %{
    "title" => "Basic operator",
    "reset_lesson_position" => false,
    "lessons" => [
      %{"name" => "Addition / Subtraction"},
      %{"name" => "Multiplication / Division"}
    ]
  },
  %{
    "title" => "Advanced topics",
    "reset_lesson_position" => true,
    "lessons" => [
      %{"name" => "Mutability"},
      %{"name" => "Immutability"}
    ]
  }
]

expected_result = [
  %{
    "title" => "Getting started",
    "reset_lesson_position" => false,
    "position" => 1,
    "lessons" => [
      %{"name" => "Welcome", "position" => 1},
      %{"name" => "Installation", "position" => 2}
    ]
  },
  %{
    "title" => "Basic operator",
    "reset_lesson_position" => false,
    "position" => 2,
    "lessons" => [
      %{"name" => "Addition / Subtraction", "position" => 3},
      %{"name" => "Multiplication / Division", "position" => 4}
    ]
  },
  %{
    "title" => "Advanced topics",
    "reset_lesson_position" => true,
    "position" => 3,
    "lessons" => [
      %{"name" => "Mutability", "position" => 1},
      %{"name" => "Immutability", "position" => 2}
    ]
  }
]

defmodule DataTraversal do
  def update_positions(sections) do
    sections
    |> put_cons(["position"], 1)
    |> chunk_when(& &1["reset_lesson_position"])
    |> Enum.flat_map(&put_cons(&1, ["lessons", "position"], 1))
  end

  # Puts consecutive values in map (potentially nested
  # in lists and other maps).
  defp put_cons(item, path, start) do
    put_cons_iterate(item, path, start) |> elem(0)
  end

  defp put_cons_iterate(item, [key], start) when is_map(item) do
    {Map.put(item, key, start), start + 1}
  end

  defp put_cons_iterate(item, [key | keys], start) when is_map(item) do
    {value, counter} = put_cons_iterate(item[key], keys, start)
    {%{item | key => value}, counter}
  end

  defp put_cons_iterate(item, path, start) when is_list(item) do
    Enum.map_reduce(item, start, &put_cons_iterate(&1, path, &2))
  end

  # Chunks the enumerable, such that every item satisfying
  # `condition` starts a new chunk.
  defp chunk_when(enumerable, condition) do
    chunk_fun = fn element, acc ->
      if condition.(element) do
        {:cont, Enum.reverse(acc), [element]}
      else
        {:cont, [element | acc]}
      end
    end

    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, Enum.reverse(acc), []}
    end

    Enum.chunk_while(enumerable, [], chunk_fun, after_fun)
  end
end

^expected_result =
  DataTraversal.update_positions(sections)
  |> IO.inspect(limit: :infinity)

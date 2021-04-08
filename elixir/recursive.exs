defmodule Solver do
  def solve([curr | map], accum, title_counter, lesson_counter) do
    curr = Map.put(curr, "position", title_counter)
    lesson_counter = if curr["reset_lesson_position"], do: 0, else: lesson_counter

    curr =
      Map.put(
        curr,
        "lessons",
        Enum.map(Enum.with_index(curr["lessons"], 1), fn {lesson, index} ->
          Map.put(lesson, "position", lesson_counter + index)
        end)
      )

    solve(map, [curr | accum], title_counter + 1, lesson_counter + Enum.count(curr["lessons"]))
  end

  def solve([], accum, _title_counter, _lesson_counter) do
    Enum.reverse(accum)
  end
end

input = [
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

test = [
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

output = Solver.solve(input, [], 1, 0)
IO.inspect(output)
true = output == test

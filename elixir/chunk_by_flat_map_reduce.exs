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

output_fun = fn input ->
  input
  |> Enum.chunk_by(& &1["reset_lesson_position"])
  |> Enum.flat_map_reduce(1, fn sections, section_offset ->
    sections
    |> Enum.with_index()
    |> Enum.map_reduce(1, fn {section, section_index}, lesson_offset ->
      lessons_map_fun = &Map.put(elem(&1, 0), "position", lesson_offset + elem(&1, 1))
      lessons = Enum.map(Enum.with_index(section["lessons"]), lessons_map_fun)
      section_position = section_index + section_offset
      section = Map.merge(section, %{"lessons" => lessons, "position" => section_position})
      {section, lesson_offset + length(lessons)}
    end)
    |> elem(0)
    |> (&{&1, section_offset + length(&1)}).()
  end)
  |> elem(0)
end

output = output_fun.(input)
true = output == test
IO.inspect(output, limit: :infinity)

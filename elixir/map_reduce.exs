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

{sections, _acc} =
  Enum.map_reduce(sections, {1, 1}, fn section, {section_counter, lesson_counter} ->
    lesson_counter = if section["reset_lesson_position"], do: 1, else: lesson_counter

    {lessons, lesson_counter} =
      Enum.map_reduce(section["lessons"], lesson_counter, fn lesson, lesson_counter ->
        {Map.put(lesson, "position", lesson_counter), lesson_counter + 1}
      end)

    section =
      section
      |> Map.put("lessons", lessons)
      |> Map.put("position", section_counter)

    {section, {section_counter + 1, lesson_counter}}
  end)

IO.inspect(sections)

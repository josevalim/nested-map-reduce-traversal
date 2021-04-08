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

{sections, _} =
  for {section, section_position} <- Enum.with_index(sections, 1), reduce: {[], 1} do
    {acc, lesson_position} ->
      lesson_position = if section["reset_lesson_position"], do: 1, else: lesson_position

      {lessons, lesson_position} =
        for {lesson, lesson_position} <- Enum.with_index(section["lessons"], lesson_position),
            reduce: {[], lesson_position} do
          {acc, _} ->
            lesson = Map.put(lesson, "position", lesson_position)
            {[lesson | acc], lesson_position}
        end

      section =
        section
        |> Map.put("lessons", Enum.reverse(lessons))
        |> Map.put("position", section_position)

      {[section | acc], lesson_position + 1}
  end

sections = Enum.reverse(sections)
IO.inspect(sections)

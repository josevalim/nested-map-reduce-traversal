defmodule FolderSolver do
  @moduledoc """
  With assumption that everything is a List, we only need to fold it right!

  As we also need to track additional data (section or lesson positions)
  between the sections, we use a two elements tuple to pass these pieces of
  information between the folds.

  Though this little tuple-hack makes us to use `elem/2` before we can return
  a true output result (as we have also to reverse accumulated lists).

  The similar approach with tuple-based accumulator we use for the lessons
  folding flow.
  """

  def solve(input) do
    input
    |> List.foldl({[], 1}, &fold_section/2)
    |> elem(0)
    |> Enum.reverse()
  end

  defp fold_section(section, {sections, lesson_position}) do
    # reset if needed, but pass the previous value if not
    lesson_position =
      case section[:reset_lesson_position] do
        true -> 1
        _ -> lesson_position
      end

    lessons =
      section[:lessons]
      |> List.foldl({[], lesson_position}, &fold_lesson/2)
      |> elem(0)
      |> Enum.reverse()

    section_position = length(sections) + 1

    section_with_position =
      section
      |> Map.put(:position, section_position)
      |> Map.put(:lessons, lessons)

    sections = [section_with_position | sections]
    lesson_position = lesson_position + length(lessons)

    {sections, lesson_position}
  end

  defp fold_lesson(lesson, {lessons, lesson_position}) do
    lesson_with_position =
      lesson
      |> Map.put(:position, lesson_position)

    {[lesson_with_position | lessons], lesson_position + 1}
  end
end

#########################
### RUN, FOREST, RUN! ###
#########################

input = [
  %{
    title: "Getting started",
    reset_lesson_position: false,
    lessons: [
      %{name: "Welcome"},
      %{name: "Installation"}
    ]
  },
  %{
    title: "Basic operator",
    reset_lesson_position: false,
    lessons: [
      %{name: "Addition / Subtraction"},
      %{name: "Multiplication / Division"}
    ]
  },
  %{
    title: "Advanced topics",
    reset_lesson_position: true,
    lessons: [
      %{name: "Mutability"},
      %{name: "Immutability"}
    ]
  }
]

FolderSolver.solve(input)
|> IO.inspect()

defmodule Sections do
  @sections [
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

  def sections(), do: @sections

  def position_lessons(original) do
    counters = :counters.new(2, [])
    :counters.put(counters, 1, 0)
    :counters.put(counters, 2, 0)

    Enum.map(original, fn %{reset_lesson_position: rlp, lessons: lessons} = section ->
      if(rlp, do: :counters.put(counters, 2, 0))
      :counters.add(counters, 1, 1)

      %{
        section
        | lessons:
            Enum.map(lessons, fn lesson ->
              :counters.add(counters, 2, 1)
              Map.put(lesson, :position, :counters.get(counters, 2))
            end)
      }
      |> Map.put(:position, :counters.get(counters, 1))
    end)
  end
end

Sections.sections()
|> Sections.position_lessons()
|> IO.inspect()

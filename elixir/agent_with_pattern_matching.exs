defmodule Sections do
  @moduledoc """
  Basically we need to keep state for the top level field :position,
  and do the same for the field :position inside each map in the list "lessons".

  As soon as we match on reset_lesson_position: true we stop Agent for lessons and starting it over again.

  Inside elixir folder run:
  - elixir agent_with_pattern_matching.exs --run (to run the implementation)
  - elixir agent_with_pattern_matching.exs --test (to test the implementation)
  """
  alias Sections.TopField
  alias Sections.Lessons

  def run() do
    TopField.start()
    Lessons.start()

    result =
      sections_input()
      |> Enum.map(&transform/1)

    TopField.stop()
    Lessons.stop()

    result
    |> IO.inspect(label: "\nRESULT \n")
  end

  # if reset_lesson_position: true -> Agent.stop and start from scratch
  defp transform(%{lessons: lessons, reset_lesson_position: true} = map) do
    Lessons.stop()
    Lessons.start()

    exec_job(map, lessons)
  end

  # if reset_lesson_position: false -> keep going
  defp transform(%{lessons: lessons, reset_lesson_position: false} = map),
    do: exec_job(map, lessons)

  # updating lessons list and inserting top level field
  defp exec_job(map, lessons) do
    lessons_updated =
      Enum.map(lessons, fn lesson ->
        update_map(lesson, :lessons)
      end)

    update_map(map, :top)
    |> Map.put(:lessons, lessons_updated)
  end

  # updating map with top level :position field
  defp update_map(map, :top) do
    TopField.update()
    Map.put(map, :position, TopField.get())
  end

  # updating each map inside lessons list with :position field
  defp update_map(map, :lessons) do
    Lessons.update()
    Map.put(map, :position, Lessons.get())
  end

  # input data
  def sections_input do
    [
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
  end
end

defmodule Sections.TopField do
  use Agent

  def start(), do: Agent.start_link(fn -> 0 end, name: :top_field)

  def update(),
    do: Agent.update(:top_field, &(&1 + 1))

  def get(), do: Agent.get(:top_field, & &1)

  def stop(), do: Agent.stop(:top_field)
end

defmodule Sections.Lessons do
  use Agent

  def start(), do: Agent.start_link(fn -> 0 end, name: :lessons)

  def update(),
    do: Agent.update(:lessons, &(&1 + 1))

  def get(), do: Agent.get(:lessons, & &1)

  def stop(), do: Agent.stop(:lessons)
end

case System.argv() do
  ["--run"] ->
    Sections.run()

  ["--test"] ->
    ExUnit.start()

    defmodule SectionsTest do
      use ExUnit.Case

      describe "Sections.run/0" do
        test "success: expected output from run/0 function" do
          assert [
                   %{
                     lessons: [
                       %{name: "Welcome", position: 1},
                       %{name: "Installation", position: 2}
                     ],
                     position: 1,
                     reset_lesson_position: false,
                     title: "Getting started"
                   },
                   %{
                     lessons: [
                       %{name: "Addition / Subtraction", position: 3},
                       %{name: "Multiplication / Division", position: 4}
                     ],
                     position: 2,
                     reset_lesson_position: false,
                     title: "Basic operator"
                   },
                   %{
                     lessons: [
                       %{name: "Mutability", position: 1},
                       %{name: "Immutability", position: 2}
                     ],
                     position: 3,
                     reset_lesson_position: true,
                     title: "Advanced topics"
                   }
                 ] = output = Sections.run()

          assert output != Sections.sections_input()
        end
      end
    end

  _ ->
    IO.puts(:stderr, "\nplease specify --run or --test")
end

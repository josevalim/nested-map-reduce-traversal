defmodule Sections do
  @table :table

  def next_lesson do
    :ets.update_counter(@table, :lessons, 1, {:lessons, 0})
  end

  def reset_lessons do
    :ets.insert(@table, {:lessons, 0})
  end

  def next_position do
    :ets.update_counter(@table, :positions, 1, {:positions, 0})
  end

  def update(sections) do
    :ets.new(@table, [:named_table, :set])

    res =
      for section <- sections do
        if section["reset_lesson_position"], do: reset_lessons()

        new_lessons =
          for lesson <- section["lessons"] do
            Map.put(lesson, "position", next_lesson())
          end

        Map.merge(section, %{"position" => next_position(), "lessons" => new_lessons})
      end

    :ets.delete(@table)
    res
  end
end

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

sections
|> Sections.update()
|> IO.inspect()

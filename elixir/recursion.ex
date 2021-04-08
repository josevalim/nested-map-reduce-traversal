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

defmodule Lessons do
  def parse(sections) do
    parse sections, 0, []
  end

  def parse([] = _sections, _last_lesson_position, acc) do
    acc |> :lists.reverse
  end

  def parse([hd|rest] = _sections, last_lesson_position, acc) do
    new_section = hd
    new_section = Map.put(new_section, "position", 1 + Enum.count(acc))
    cur_lesson_sz = Map.get(hd, "lessons") |> Enum.count
    reset_lesson_position = (Map.get(new_section, "reset_lesson_position") == true)

    new_lessons = if cur_lesson_sz > 0 do
      (0..(cur_lesson_sz - 1)) |> Enum.map(fn(idx) ->
        ll = Map.get(hd, "lessons") |> Enum.at(idx)

        lesson_cnt = if reset_lesson_position do
          (idx + 1)
        else
          last_lesson_position + (idx + 1)
        end
        new_ll = Map.put(ll, "position", lesson_cnt)
        new_ll
      end)
    else
      []
    end

    new_section = Map.put(new_section, "lessons", new_lessons)

    if reset_lesson_position do
      parse rest, cur_lesson_sz, [new_section|acc]
    else
      parse rest, last_lesson_position + cur_lesson_sz, [new_section|acc]
    end
  end
end

ss = Lessons.parse sections
IO.inspect(ss)

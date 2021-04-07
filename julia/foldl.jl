sections = [
  Dict(
    "title" => "Getting started",
    "reset_lesson_position" => false,
    "lessons" => [
      Dict("name" => "Welcome"),
      Dict("name" => "Installation")
    ]
  ),
  Dict(
    "title" => "Basic operator",
    "reset_lesson_position" => false,
    "lessons" => [
      Dict("name" => "Addition / Subtraction"),
      Dict("name" => "Multiplication / Division")
    ]
  ),
  Dict(
    "title" => "Advanced topics",
    "reset_lesson_position" => true,
    "lessons" => [
      Dict("name" => "Mutability"),
      Dict("name" => "Immutability")
    ]
  )
]

struct Lesson
  name::String
  position::Int
end

struct Section
  title::String
  reset_lesson_position::Bool
  position::Int
  lessons::Vector{Lesson}
end

traversed_sections, _ = foldl(enumerate(sections); init=([], 0)) do (traversed_sections, lesson_counter), (section_pos, section)
  reset_lesson_position = section["reset_lesson_position"]

  if reset_lesson_position
    lesson_counter = 0
  end

  new_section = Section(
    section["title"],
    reset_lesson_position,
    section_pos,
    map(enumerate(section["lessons"])) do (lesson_pos, lesson)
      Lesson(lesson["name"], lesson_counter + lesson_pos)
    end
  )

  (
    [traversed_sections..., new_section],
    lesson_counter + length(new_section.lessons)
  )
end

dump(traversed_sections)

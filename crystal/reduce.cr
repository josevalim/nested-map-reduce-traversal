sections = [
  {
    "title"                 => "Getting started",
    "reset_lesson_position" => false,
    "lessons"               => [
      {"name" => "Welcome"},
      {"name" => "Installation"},
    ],
  },

  {
    "title"                 => "Basic operator",
    "reset_lesson_position" => false,
    "lessons"               => [
      {"name" => "Addition / Subtraction"},
      {"name" => "Multiplication / Division"},
    ],
  },

  {
    "title"                 => "Advanced topics",
    "reset_lesson_position" => true,
    "lessons"               => [
      {"name" => "Mutability"},
      {"name" => "Immutability"},
    ],
  },
]

alias Lesson = Hash(String, String | Int32)
alias Section = Hash(String, Array(Lesson) | Array(Hash(String, String)) | Bool | Int32 | String)

section_counter = 1
lesson_counter = 1

result = sections.reduce([] of Section) do |memo, section|
  lessons = [] of Lesson

  lesson_counter = 1 if section["reset_lesson_position"]

  section["lessons"].as(Array).each do |lesson|
    lessons << lesson.merge({"position" => lesson_counter})
    lesson_counter += 1
  end

  memo << section
    .select("title", "reset_lesson_position")
    .merge({"position" => section_counter, "lessons" => lessons})

  section_counter += 1

  memo
end

pp result

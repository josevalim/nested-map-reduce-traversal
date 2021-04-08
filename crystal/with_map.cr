sections = [
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Welcome"},
      {"name": "Installation"}
    ]
  },

  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Addition / Subtraction"},
      {"name": "Multiplication / Division"}
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      {"name": "Mutability"},
      {"name": "Immutability"}
    ]
  }
]

def indexing_lessons(lessons : Array)
  lessons.map_with_index(1) { |lesson, index| lesson.merge({ position: index}) }
end

selected_lessons = sections.flat_map(&.[:lessons])
indexed_lessons = indexing_lessons(selected_lessons)

formated_section = sections.map_with_index(1) do |section, i|
  formated_lessons = indexed_lessons.shift(section[:lessons].size)
  if section[:reset_lesson_position]
    formated_lessons = indexing_lessons(section[:lessons])
  end
  section.merge(position: i, lessons: formated_lessons)
end

pp formated_section

require 'json'

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

section_counter = 1
lesson_counter = 1

sections.each do |section|
  lesson_counter = 1 if section[:reset_lesson_position]
  
  section[:position] = section_counter
  section_counter += 1

  section[:lessons].each do |lesson|
    lesson[:position] = lesson_counter
    lesson_counter += 1
  end
end

puts JSON.dump(sections)
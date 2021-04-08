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

reset_lesson_position = lambda {|lessons| lessons.each.with_index(1) { |lesson, index|  lesson.merge!(position: index) } }

modify_section = lambda do |section, indexed_lessons|
  lessons = section[:reset_lesson_position] ? reset_lesson_position.call(section[:lessons]) : indexed_lessons
  section.merge!(lessons: lessons) 
end

indexed_sections = sections.flat_map{ _1[:lessons] }.each.with_index(1) { |a, index|  a.merge!(position: index) }
sections.map { |section| modify_section.call(section, indexed_sections.shift(section[:lessons].count)) }

puts JSON.pretty_generate(sections)

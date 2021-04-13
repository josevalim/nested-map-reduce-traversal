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

def add_positions(section, section_position, lessons_offset)
  section[:position] = section_position
  section[:lessons].each.with_index(lessons_offset) { |l, pos| l[:position] = pos }
end

# bootstrap first section
add_positions(sections.first, 1, 1)

# iterate consecutively by twos starting index at section position 2
sections.each_cons(2).with_index(2) do |(prior_section, current_section), section_position|
  lesson_offset = 1
  # add to offset if necessary
  lesson_offset += prior_section.dig(:lessons, -1, :position) unless current_section[:reset_lesson_position]
  add_positions(current_section, section_position, lesson_offset)
end

puts JSON.dump(sections)

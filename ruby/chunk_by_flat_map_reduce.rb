# for JSON pretty format
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

output = sections.each_with_index.chunk_while {|(i, _idxi), (j, idxj)| !j[:reset_lesson_position] }.flat_map do |chunk|
  chunk_lesson_idx = 0
  chunk.map do |section, section_idx|
    section.merge(
      position: section_idx + 1,
      lessons: section[:lessons].each_with_index.map do |lesson, lesson_idx|
        lesson.merge(position: chunk_lesson_idx += 1)
      end
    )
  end
end

puts JSON.pretty_generate(output)

# double check result
test = [
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "position": 1,
    "lessons": [
      {"name": "Welcome", "position": 1},
      {"name": "Installation", "position": 2}
    ]
  },
  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "position": 2,
    "lessons": [
      {"name": "Addition / Subtraction", "position": 3},
      {"name": "Multiplication / Division", "position": 4}
    ]
  },
  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "position": 3,
    "lessons": [
      {"name": "Mutability", "position": 1},
      {"name": "Immutability", "position": 2}
    ]
  }
]

if output != test
  raise "output not equal expected"
end

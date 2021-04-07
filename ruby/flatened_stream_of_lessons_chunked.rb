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
  },
]

# unwinding into a flat lesson stream
lesson_section_stream = sections.each_with_index.flat_map do |section, section_idx|
  section_root = section.slice(*section.keys - [:lessons])
  section_root[:position] = section_idx + 1

  section[:lessons].map do |lesson|
    {
      lesson: lesson,
      section: section_root,
    }
  end
end

# chunk lessons by reset position
chunked_lesson_section_stream = lesson_section_stream.chunk_while {|l1, l2| l1[:section][:position] == l2[:section][:position] || !l2[:section][:reset_lesson_position] }

# map lessons with position
positioned_lesson_stream = chunked_lesson_section_stream.flat_map do |lesson_section_chunk|
  lesson_section_chunk.each_with_index.map do |lesson_section, lesson_idx|
    {
      lesson: lesson_section[:lesson].merge(position: lesson_idx + 1),
      section: lesson_section[:section]
    }
  end
end

# reduce output to section groups
output = positioned_lesson_stream.group_by{|l| l[:section] }.map{|k, v| k.merge(lessons: v.map{|i| i[:lesson] })}

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

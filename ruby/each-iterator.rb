# for JSON pretty format
require 'json'

# intenationally using string keys
sections = [
  {
    "title" => "Getting started",
    "reset_lesson_position" => false,
    "lessons" => [
      {"name" => "Welcome"},
      {"name" => "Installation"}
    ]
  },

  {
    "title" => "Basic operator",
    "reset_lesson_position" => false,
    "lessons" => [
      {"name" => "Addition / Subtraction"},
      {"name" => "Multiplication / Division"}
    ]
  },

  {
    "title" => "Advanced topics",
    "reset_lesson_position" => true,
    "lessons" => [
      {"name" => "Mutability"},
      {"name" => "Immutability"}
    ]
  }
]

# want to see something neat?
base_counter = 1.upto(Float::INFINITY)
counter = base_counter.dup

sections.each_with_index do |section, section_index|
  # you can mutate the section in place it won't affect the iteration
  section["position"] = section_index + 1

  if section["reset_lesson_position"]
    # equivalent to resetting it to 1
    counter = base_counter.dup
  end

  section["lessons"].each do |lesson|
    # you can also mutate the lesson in place
    lesson["position"] = counter.next
  end
end

puts JSON.pretty_generate(sections)

# frozen_string_literal: true

INPUT = [
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
].freeze

EXPECTED_OUTPUT = [
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "position": 1,
    "lessons": [
      {"name": "Welcome", "position": 1},
      {"name": "Installation", "position": 2},
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
].freeze

# Returns a range used to pick the position elements for a given section.
# In case it's the first element, then it starts from it, otherwise
# calculates the begin and end position from the previous section.
# 
# @param index [Integer] the index from the current section.
# @param section [Hash<Symbol, <Array<Hash>, Boolean, Integer, String>>] the current section.
# @param sections [Array<Hash>] all the partition sections.
# 
# @return [Range<Integer>] the begin and end to slice the section positioned lessons.
def slicing_items_range(index, section, sections)
  return 0...section[:lessons].size if index.zero?

  n_prev_lessons = sections[index.pred][:lessons].size
  n_prev_lessons...n_prev_lessons + section[:lessons].size
end

# Receives the partitioned sections and maps their lessons assigning them a
# position depending on their index within the flattened array starting from 1.
#
# @param sections [Array<Hash>] all the partition sections.
#
# @return [Array<Hash<Symbol, <Integer, String>>>]
#   the array of lessons with their corresponding position key/value.
def positioned_lessons(sections)
  sections
    .map { |section| section[:lessons] }
    .flatten.map.with_index(1) { |lesson, index| lesson.merge(position: index) }
end

# Maps the sections to a new hash containing their initial lessons
# but now with their assigined position within the partition.
#
# @param sections [Array<Hash>] all the partition sections.
# @param positioned_lessons [Array<Hash>] the partition lessons alredy with their assigned position.
#
# @return [Array<Hash>]
#   an array containing the partition sections with their lessons and respective positions.
def assigned_positioned_lessons(sections, positioned_lessons)
  sections
    .map.with_index { |section, index| [section, slicing_items_range(index, section, sections)] }
    .map { |section, range| [section, positioned_lessons[range]] }
    .map { |section, new_lessons| section.merge(lessons: new_lessons) }
end

# Receives an array of hashes (sections) and adds them a position
# key which is the index each ahsh has in the input parameter.
#
# It partitions the array by the reset_lesson_position value of their elements,
# and then maps them to assign them their lessons with their corresponding position.
#
# Finally the result is flattened and sorted by the position value of each section.
#
# @param input [Array<Hash<Symbol, <Array<Hash>, Boolean, Integer, String>>>]
def traverse_nested_data_structure(input)
  input
    .map.with_index(1) { |section, index| section.merge(position: index) }
    .partition { |section| section[:reset_lesson_position] == true }
    .map { |sections| [sections, positioned_lessons(sections)] }
    .flat_map { |sections, positioned_lessons| assigned_positioned_lessons(sections, positioned_lessons) }
    .sort_by { |section| section[:position] }
end

p traverse_nested_data_structure(INPUT) == EXPECTED_OUTPUT
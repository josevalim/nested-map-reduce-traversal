# frozen_string_literal: true

require 'json'

MAIN_STRUCTURE = [
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

def map_each
  counter = 1

  MAIN_STRUCTURE.map.with_index(1) { |section, index| section.merge!(position: index) }.each do |hash|
    counter = 1 if hash[:reset_lesson_position]
    hash[:lessons].each { |lesson| lesson[:position] = counter; counter += 1 }
  end
end

puts JSON.pretty_generate map_each

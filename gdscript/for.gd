extends Node

var sections : Array = [
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "lessons": [
      { "name": "Welcome" },
      { "name": "Installation" }
    ]
  },
  
  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "lessons": [
      { "name": "Addition / Subtraction" },
      { "name": "Multiplication / Division" }
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      { "name": "Mutability" },
      { "name": "Immutability" }
    ]
  }
]

var lesson_position : int = 0
	
func _ready() -> void:
  for index in sections.size():
    var section : Dictionary = sections[index]
	section.position = index + 1
	
	if section.reset_lesson_position: lesson_position = 0

    for lesson in section.lessons:
	  lesson_position += 1
	  lesson.position = lesson_position
  
  print(sections)

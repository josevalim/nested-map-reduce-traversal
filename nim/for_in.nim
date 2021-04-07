import json

let sections = """[
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
]""".parseJson
var
  sectionCounter = 1
  lessonCounter = 1

for section in sections:
  if section["reset_lesson_position"].getBool:
    lessonCounter = 1

  section["position"] = sectionCounter.newJInt
  sectionCounter.inc()

  for lesson in section["lessons"].getElems:
    lesson["position"] = lessonCounter.newJInt
    lessonCounter.inc()

echo sections

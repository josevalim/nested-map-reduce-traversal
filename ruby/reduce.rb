original_json = [
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

positioned_json = original_json.reduce([]) do |memo, obj|
  obj[:position] = memo.length + 1

  last_obj = memo.last || {}
  last_lesson_index = 0
  unless obj[:reset_lesson_position]
    last_lesson_index = last_obj[:lessons]&.last&.dig(:position) || 0
  end
  next_lesson_index = last_lesson_index + 1

  obj[:lessons] = obj[:lessons].reduce([]) do |lessons, lesson|
    lesson[:position] = lessons.length + next_lesson_index
    last_lesson_index = lesson[:position]

    lessons << lesson
    lessons
  end

  memo << obj
  memo
end

pp positioned_json

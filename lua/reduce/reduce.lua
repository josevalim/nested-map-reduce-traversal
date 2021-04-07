local sections = {
  {
    title = "Getting started",
    reset_lesson_position = false,
    lessons = {
      {name = "Welcome"},
      {name = "Installation"}
    }
  },

  {
    title = "Basic operator",
    reset_lesson_position = false,
    lessons = {
      {name = "Addition / Subtraction"},
      {name = "Multiplication / Division"}
    }
  },

  {
    title = "Advanced topics",
    reset_lesson_position = true,
    lessons = {
      {name = "Mutability"},
      {name = "Immutability"}
    }
  }
}

-- the easy part
local lesson_index = 1
for index,section in ipairs(sections) do
  section.position = index

  if section.reset_lesson_position then
    lesson_index = 1
  end

  for _,lesson in ipairs(section.lessons) do
    lesson.position = lesson_index
    lesson_index = lesson_index + 1
  end
end

local json = require('json')

print(json.encode(sections))

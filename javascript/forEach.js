const sections = [
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
];

let lessonPos = 1;
sections.forEach((section, idx) => {
  if (section.reset_lesson_position) lessonPos = 1;
  section.position = idx + 1;
  section.lessons.forEach(lesson => lesson.position = lessonPos++);
});

console.dir(sections, {depth:3});

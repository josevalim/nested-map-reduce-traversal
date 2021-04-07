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

let sectionCounter = 1;
let lessonCounter = 1;
for(const section of sections){
  if(section.reset_lesson_position){
    lessonCounter = 1
  }
  section.position = sectionCounter++;
  for(const lesson of section.lessons){
    lesson.position = lessonCounter++;
  }
}

console.log(JSON.stringify(sections, null, 2))

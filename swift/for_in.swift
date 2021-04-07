class Section {
  let title: String
  let resetLessonPosition: Bool
  let lessons: [Lesson]
  var position: Int?

  init(_ title: String, _ resetLessonPosition: Bool, _ lessons: [Lesson]) {
    self.title = title
    self.resetLessonPosition = resetLessonPosition
    self.lessons = lessons
  }
}

class Lesson {
  let name: String
  var position: Int?
  init(_ name: String) {
    self.name = name
  }
}

var sections = [
  Section("Getting started", false,
          [Lesson("Welcome"),
           Lesson("Instalation")]),
  Section("Basic operator", false, [
          Lesson("Addition / Subtraction"),
          Lesson("Multiplication / Division")]),
  Section("Advanced topics", true, [
          Lesson("Mutability"),
          Lesson("Immutability")])
]

var lessonCounter = 1
var sectionCounter = 1

for section in sections {
  if section.resetLessonPosition {
    lessonCounter = 0
  }
  section.position = sectionCounter
  sectionCounter += 1
  for lesson in section.lessons {
    lesson.position = lessonCounter
    lessonCounter += 1
  }
}

debugPrint(sections)

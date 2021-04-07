import nested.{Lesson, PositionedLesson, PositionedSubject, Subject}
import gleam/should

pub fn main_test() {
  [
    Subject(
      title: "Getting started",
      reset_lesson_position: False,
      lessons: [Lesson(name: "Welcome"), Lesson(name: "Installation")],
    ),
    Subject(
      title: "Basic operator",
      reset_lesson_position: False,
      lessons: [
        Lesson(name: "Addition / Subtraction"),
        Lesson(name: "Multiplication / Division"),
      ],
    ),
    Subject(
      title: "Advanced topics",
      reset_lesson_position: True,
      lessons: [Lesson(name: "Mutability"), Lesson(name: "Immutability")],
    ),
  ]
  |> nested.main
  |> should.equal([
    PositionedSubject(
      title: "Getting started",
      reset_lesson_position: False,
      position: 1,
      lessons: [
        PositionedLesson(name: "Welcome", position: 1),
        PositionedLesson(name: "Installation", position: 2),
      ],
    ),
    PositionedSubject(
      title: "Basic operator",
      reset_lesson_position: False,
      position: 2,
      lessons: [
        PositionedLesson(name: "Addition / Subtraction", position: 3),
        PositionedLesson(name: "Multiplication / Division", position: 4),
      ],
    ),
    PositionedSubject(
      title: "Advanced topics",
      reset_lesson_position: True,
      position: 3,
      lessons: [
        PositionedLesson(name: "Mutability", position: 1),
        PositionedLesson(name: "Immutability", position: 2),
      ],
    ),
  ])
}

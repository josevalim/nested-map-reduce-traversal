import list
import gleam/optional.{Option, Some}

pub type Lesson {
  Lesson(name: String, position: Option(Int))
}

pub type Subject {
  Subject(
    title: String,
    reset_lesson_position: Bool,
    position: Option(Int),
    lessons: List(Lesson),
  )
}

type Positions {
  Positions(subject: Int, lesson: Int)
}

pub fn main(input) {
  list.map_reduce(over: input, from: Positions(1, 1), with: reduce).0
}

fn reduce(subject: Subject, positions: Positions) {
  let lesson_position = case subject.reset_lesson_position {
    True -> 1
    False -> positions.lesson
  }

  let tuple(lessons, lesson_position) =
    list.map_reduce(
      over: subject.lessons,
      from: lesson_position,
      with: fn(lesson: Lesson, position) {
        tuple(Lesson(..lesson, position: Some(position)), position + 1)
      },
    )

  tuple(
    Subject(..subject, lessons: lessons, position: Some(positions.subject)),
    Positions(positions.subject + 1, lesson_position),
  )
}

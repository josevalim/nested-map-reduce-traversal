import list

pub type Lesson {
  Lesson(name: String)
}

pub type PositionedLesson {
  PositionedLesson(name: String, position: Int)
}

pub type Subject {
  Subject(title: String, reset_lesson_position: Bool, lessons: List(Lesson))
}

pub type PositionedSubject {
  PositionedSubject(
    title: String,
    reset_lesson_position: Bool,
    position: Int,
    lessons: List(PositionedLesson),
  )
}

type Positions {
  Positions(subject: Int, lesson: Int)
}

pub fn main(input: List(Subject)) -> List(PositionedSubject) {
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
        tuple(PositionedLesson(lesson.name, position), position + 1)
      },
    )

  let subject =
    PositionedSubject(
      title: subject.title,
      reset_lesson_position: subject.reset_lesson_position,
      position: positions.subject,
      lessons: lessons,
    )

  tuple(subject, Positions(positions.subject + 1, lesson_position))
}

import Foundation

struct InputSection {
    let title: String
    let resetLessonPosition: Bool
    let lessons: [InputLesson]
}

struct InputLesson {
    let name: String
}

let input = [
    InputSection(
        title: "Getting started",
        resetLessonPosition: false,
        lessons: [
            InputLesson(name: "Welcome"),
            InputLesson(name: "Instalation")
        ]
    ),
    InputSection(
        title: "Basic operator",
        resetLessonPosition: false,
        lessons: [
            InputLesson(name: "Addition / Subtraction"),
            InputLesson(name: "Multiplication / Division")
        ]
    ),
    InputSection(
        title: "Advanced topics",
        resetLessonPosition: true,
        lessons: [
            InputLesson(name: "Mutability"),
            InputLesson(name: "Immutability")
        ]
    )
]

struct Section {
    let title: String
    let resetLessonPosition: Bool
    let position: Int
    let lessons: [Lesson]
}

struct Lesson {
    let name: String
    let position: Int
}

var lessonCounter = 1

let sections = input.enumerated().map { index, inputSection -> Section in
    if inputSection.resetLessonPosition {
        lessonCounter = 1
    }
    return Section(
        title: inputSection.title,
        resetLessonPosition: inputSection.resetLessonPosition,
        position: index + 1,
        lessons: inputSection.lessons.map { inputLesson in
            let lesson = Lesson(
                name: inputLesson.name,
                position: lessonCounter
            )
            lessonCounter += 1
            return lesson
        }
    )
}

debugPrint(sections)

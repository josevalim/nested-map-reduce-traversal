data class Section(
    val title: String,
    val resetLessonPosition: Boolean,
    val lessons: List<Lesson>,
    var position: Int? = null
)

data class Lesson(val name: String, var position: Int? = null)

val sections = listOf(
    Section(
        "Getting started", false, listOf(
            Lesson("Welcome"),
            Lesson("Installation"),
        )
    ),
    Section(
        "Basic operator", false, listOf(
            Lesson("Addition / Subtraction"),
            Lesson("Multiplication / Division"),
        )
    ),
    Section(
        "Advanced topics", true, listOf(
            Lesson("Mutability"),
            Lesson("Immutability"),
        )
    )
)


fun main() {
    var lessonCounter = 1
    var sectionCounter = 1
    sections.forEach { section ->
        if (section.resetLessonPosition) {
            lessonCounter = 1
        }
        section.position = sectionCounter
        sectionCounter++
        section.lessons.forEach { lesson ->
            lesson.position = lessonCounter
            lessonCounter++
        }

    }

    println(sections)
}

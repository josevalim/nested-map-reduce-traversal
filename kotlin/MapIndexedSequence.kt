data class Section(
    val title: String,
    val resetLessonPosition: Boolean,
    val lessons: List<Lesson>,
    val position: Int? = null
)

data class Lesson(val name: String, val position: Int? = null)

private val sections = listOf(
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

private fun positions(): Sequence<Int> = generateSequence(1) { it + 1 }

fun main() {
    var lessonNumbers = positions().iterator()

    val updated = sections.mapIndexed { idx, section ->
        if (section.resetLessonPosition) lessonNumbers = positions().iterator()
        section.copy(
            lessons = section.lessons.map { it.copy(position = lessonNumbers.next()) },
            position = idx + 1
        )
    }

    println(updated)
}
type lesson  = { position: int option, name: string }
type section = { position: int option, title: string, reset_lesson_position: bool, lessons: lesson list }

fun mkSection title reset lessons = { position=NONE, title=title, reset_lesson_position=reset, lessons=lessons }
fun mkLesson name                 = { position=NONE, name=name }

fun setPosLessons pos lessons {title, reset_lesson_position, ...} =
    { position=SOME pos, lessons=lessons, title=title, reset_lesson_position=reset_lesson_position }
fun setPos pos {name, ...} =
    { position=SOME pos, name=name }

val sections : section list = [
    mkSection "Getting started" false [
        mkLesson "Welcome",
        mkLesson "Installation"
    ],
    mkSection "Basic operator" false [
        mkLesson "Addition / Subtraction",
        mkLesson "Multiplication / Division"
    ],
    mkSection "Advanced topics" true [
        mkLesson "Mutability",
        mkLesson "Immutability"
    ]
]

fun withPositions (sections : section list) =
    (#1 o positionSections) sections

and positionSections sections =
    List.foldl positionSection ([], 1, 1) sections

and positionSection (section as { title, reset_lesson_position, lessons, ... }, (sections, secPos, lessPos)) =
    let val lessPos = (if reset_lesson_position then 1 else lessPos)
        val (lessons, lessPos) = positionLessons lessPos lessons in
        (sections @ [setPosLessons secPos lessons section], secPos + 1, lessPos)
    end

and positionLessons start lessons =
    List.foldl positionLesson ([], start) lessons

and positionLesson (lesson, (lessons, lessPos)) =
    (lessons @ [setPos lessPos lesson], lessPos + 1)

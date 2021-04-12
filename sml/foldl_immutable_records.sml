type lesson  = { position: int option, name: string }
type section = { position: int option, title: string, reset_lesson_position: bool, lessons: lesson list }

fun mkSection title reset lessons = { position=NONE, title=title, reset_lesson_position=reset, lessons=lessons }
fun mkLesson name                 = { position=NONE, name=name }

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
    (rev o #1 o positionSections) sections

and positionSections sections =
    List.foldl positionSection ([], 1, 1) sections

and positionSection ({ title, reset_lesson_position, lessons, ... }, (acc, secPos, lessPos)) =
    let val (ls, lessPos) = positionLessons (if reset_lesson_position then 1 else lessPos) lessons
        val section = { position=secPos, title=title, reset_lesson_position=reset_lesson_position, lessons=rev ls } in
        (section :: acc, secPos + 1, lessPos)
    end

and positionLessons start lessons =
    List.foldl positionLesson ([], start) lessons

and positionLesson ({name : string, ...}, (acc, pos)) =
    ({ position=SOME pos, name=name } :: acc, pos + 1)

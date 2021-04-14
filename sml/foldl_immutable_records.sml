type lesson  = { position: int option, name: string }
type section = { position: int option, title: string, reset_lesson_position: bool, lessons: lesson list }

fun mkSection title reset lessons = { position=NONE, title=title, reset_lesson_position=reset, lessons=lessons }
fun mkLesson name                 = { position=NONE, name=name }

fun setPosLessons pos lessons ({title, reset_lesson_position, ...} : section) =
    { position=SOME pos, lessons=lessons, title=title, reset_lesson_position=reset_lesson_position }
fun setPos pos ({name, ...} : lesson) =
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


fun withPositions sections =
    let val (revSections, _, _) = List.foldl positionSection ([], 1, 1) sections
    in  rev revSections
    end

and positionSection (section as { title, reset_lesson_position, lessons, ... }, (revSections, secPos, lessPos)) =
    let val lessPos = (if reset_lesson_position then 1 else lessPos)
        val (revLessons, lessPos) = List.foldl positionLesson ([], lessPos) lessons in
        (setPosLessons secPos (rev revLessons) section :: revSections, secPos + 1, lessPos)
    end

and positionLesson (lesson, (revLessons, lessPos)) =
    (setPos lessPos lesson :: revLessons, lessPos + 1)

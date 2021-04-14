type lesson  = {position: int option, name: string}
type section = {position: int option, title: string, resetLessonPosition: bool, lessons: lesson list}

fun mkSection title reset lessons = {position = NONE, title = title, resetLessonPosition = reset, lessons = lessons}
fun mkLesson name                 = {position = NONE, name = name}

fun updateSection pos lessons ({title, resetLessonPosition, ...} : section) =
    {position = SOME pos, lessons = lessons, title = title, resetLessonPosition = resetLessonPosition}
fun updateLesson pos ({name, ...} : lesson) =
    {position = SOME pos, name = name}

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

and positionSection (section as {title, resetLessonPosition, lessons, ...}, (revSections, secPos, lessPos)) =
    let val lessPos'1 = if resetLessonPosition then 1 else lessPos
        val (revLessons, lessPos') = List.foldl positionLesson ([], lessPos'1) lessons
    in  (updateSection secPos (rev revLessons) section :: revSections, secPos + 1, lessPos')
    end

and positionLesson (lesson, (revLessons, lessPos)) =
    (updateLesson lessPos lesson :: revLessons, lessPos + 1)


val sections' = withPositions sections

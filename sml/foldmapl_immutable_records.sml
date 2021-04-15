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


(* Define a fold-map utility function.
 * `foldMapl` is proposed for the SML Basis Library - see
 * https://github.com/SMLFamily/BasisLibrary/wiki/2015-003b-List
 * The definition below is from the section Discussion.
 *)
fun foldMapl f init xs =
  let
    fun f' (x, (ys, r)) = let val (y, r) = f (x, r) in (y :: ys, r) end
    val (ys, r) = List.foldl f' ([], init) xs
  in
    (List.rev ys, r)
  end


fun withPositions sections =
    let val (sections', _) = foldMapl positionSection (1, 1) sections
    in  sections'
    end

and positionSection (section as {title, resetLessonPosition, lessons, ...}, (secPos, lessPos)) =
    let val lessPos'1 = if resetLessonPosition then 1 else lessPos
        val (lessons', lessPos') = foldMapl positionLesson lessPos'1 lessons
    in  (updateSection secPos lessons' section, (secPos + 1, lessPos'))
    end

and positionLesson (lesson, lessPos) =
    (updateLesson lessPos lesson, lessPos + 1)


val sections' = withPositions sections

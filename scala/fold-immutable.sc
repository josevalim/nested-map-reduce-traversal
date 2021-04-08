case class Lesson(name: String, position: Option[Int] = None)
case class Section(title: String, resetLessonPosition: Boolean, lessons: List[Lesson],position: Option[Int] = None)

val  sections = List(
  Section("Getting started", false, List(
    Lesson("Welcome"),
    Lesson("Installation")
  )),
  Section("Basic operator", false,  List(
    Lesson("Addition / Subtraction"),
    Lesson("Multiplication / Division"),
  )),
  Section("Advanced topics", true, List(
    Lesson("Mutability"),
    Lesson("Immutability"),
  ))
)


val (newSections,_,_) = sections.foldLeft((List[Section](),0,0)){
  case ((sections,sectionCounter :Int,lessonCounter : Int),current) =>
    val nextSession=sectionCounter + 1
    val lessonBase = if (current.resetLessonPosition) 0 else lessonCounter
    val (lessons,nextLesson)=current.lessons.foldLeft((List[Lesson](),lessonBase)){
      case ((lessons,lessonCounter :Int),currentLesson ) =>
        val nextLesson = lessonCounter + 1
        (lessons :+ currentLesson.copy(position=Some(nextLesson)),nextLesson)
    }
    (sections :+ current.copy(position = Some(nextSession),lessons=lessons),nextSession,nextLesson)
}
newSections
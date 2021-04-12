type Lesson = { Name: string ; Position: int option }

type Section = { Title: string
                 ResetLessonPosition: bool
                 Position: int option
                 Lessons: Lesson list }

let input: Section list = [
  { Title = "Getting started"
    ResetLessonPosition = false
    Position = None 
    Lessons = [ { Name = "Welcome" ; Position = None }
                { Name = "Installation" ; Position = None } ] }
  { Title = "Basic operator"
    ResetLessonPosition = false
    Position = None 
    Lessons = [ { Name = "Addition / Subtraction" ; Position = None }
                { Name = "Multiplication / Division" ; Position = None } ] }
  { Title = "Advanced topics"
    ResetLessonPosition = true
    Position = None 
    Lessons = [ { Name = "Mutability" ; Position = None }
                { Name = "Immutability" ; Position = None } ] }
]

let expected: Section list = [
  { Title = "Getting started"
    ResetLessonPosition = false
    Position = Some 1 
    Lessons = [ { Name = "Welcome" ; Position = Some 1 }
                { Name = "Installation" ; Position = Some 2 } ] }
  { Title = "Basic operator"
    ResetLessonPosition = false
    Position = Some 2 
    Lessons = [ { Name = "Addition / Subtraction" ; Position = Some 3 }
                { Name = "Multiplication / Division" ; Position = Some 4 } ] }
  { Title = "Advanced topics"
    ResetLessonPosition = true
    Position = Some 3 
    Lessons = [ { Name = "Mutability" ; Position = Some 1 }
                { Name = "Immutability" ; Position = Some 2 } ] }
]

let optPlus n = Option.defaultValue 0 >> ((+) n) >> Some

let updateLesson start idx lesson : Lesson = { lesson with Position = start |> optPlus (idx + 1) }

let updateSection lastSection newSection : Section =
  let start = if newSection.ResetLessonPosition
              then None
              else (List.last lastSection.Lessons).Position

  { newSection with
    Position = lastSection.Position |> optPlus 1
    Lessons = newSection.Lessons
              |> List.mapi (updateLesson start) }

let updateSections input : Section list =
  input |> List.scan updateSection (List.head input) |> List.skip 1

expected = updateSections input
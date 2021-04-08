let input = [
    {| Title = "Getting started"
       ResetLessonPosition = false
       Lessons = [ {| Name = "Welcome" |}
                   {| Name = "Installation" |} ] |}

    {| Title = "Basic operator"
       ResetLessonPosition = false
       Lessons = [ {| Name = "Addition / Subtraction" |}
                   {| Name = "Multiplication / Division" |} ] |}

    {| Title = "Advanced topics"
       ResetLessonPosition = true
       Lessons = [ {| Name = "Mutability" |}
                   {| Name = "Immutability" |} ] |}
]

module Seq =
    let rec infiniteStartingAt (i: int) =
        seq {
            yield i
            while true do
                yield! infiniteStartingAt (i + 1)
        }

    let indexedStartingAt (i: int) =
        Seq.zip (infiniteStartingAt i)


let mutableSolution =
    let mutable lessonCounter = 0

    seq {
        for sectionCounter, section in (Seq.indexedStartingAt 1 input) do

            if section.ResetLessonPosition then
                lessonCounter <- 0

            {| section with Position = sectionCounter
                            Lessons = [ for lesson in section.Lessons do
                                        lessonCounter <- lessonCounter + 1
                                        {| lesson with Position = lessonCounter |} ] |}
    } |> List.ofSeq // Seq's are lazy, convert to list to calculate adn display the result.

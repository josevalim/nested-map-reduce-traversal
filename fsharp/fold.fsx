#r "nuget: FSharp.Data"

open FSharp.Data
open FSharp.Data.JsonExtensions

let private addToJsonObj key value (obj : JsonValue) =
    obj.Properties
    |> Array.filter (fun (k,_) -> k <> key)
    |> Array.append [| key, value |]
    |> JsonValue.Record


let processInput (input : JsonValue) =
    input.AsArray()
    |> Array.fold
        (fun (i ,j , state) (section : JsonValue) ->
            let resetPosition =
                section.GetProperty("reset_lesson_position") = JsonValue.Boolean true

            let lessons = section.GetProperty("lessons").AsArray()
            let lessonStartingNum = if resetPosition then 0 else j

            let newLessons =
                lessons
                |> Array.mapi
                    (fun lessonNum lesson ->
                        lesson
                        |> addToJsonObj "position"
                            ((lessonStartingNum + lessonNum + 1) |> decimal |> JsonValue.Number))

            let newSection =
                section
                |> addToJsonObj "position" (JsonValue.Number <| decimal (i + 1))
                |> addToJsonObj "lessons" (JsonValue.Array newLessons)

            (i + 1, lessonStartingNum + Array.length newLessons, newSection :: state)
        )
        (0,0,List.empty)
    |> fun (_,_,list) -> List.rev list

let json = """
[
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Welcome"},
      {"name": "Installation"}
    ]
  },

  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Addition / Subtraction"},
      {"name": "Multiplication / Division"}
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      {"name": "Mutability"},
      {"name": "Immutability"}
    ]
  }
]"""

let processed =
    json
    |> JsonValue.Parse
    |> processInput

printfn "%A" (processed.ToString())

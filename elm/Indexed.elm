module Indexed exposing (main)

import Html exposing (Html)
import List.Extra


type alias Section =
    { title : String
    , resetLessonPosition : Bool
    , lessons : List Lesson
    }


type alias Lesson =
    { name : String
    }


type alias PositionedSection =
    { title : String
    , resetLessonPosition : Bool
    , lessons : List PositionedLesson
    , position : Int
    }


type alias PositionedLesson =
    { name : String
    , position : Int
    }


main =
    output
        |> viewList viewSection


output : List PositionedSection
output =
    process input


input : List Section
input =
    [ { title = "Getting started"
      , resetLessonPosition = False
      , lessons =
            [ { name = "Welcome" }
            , { name = "Installation" }
            ]
      }
    , { title = "Basic operator"
      , resetLessonPosition = False
      , lessons =
            [ { name = "Addition / Subtraction" }
            , { name = "Multiplication / Division" }
            ]
      }
    , { title = "Advanced topics"
      , resetLessonPosition = True
      , lessons =
            [ { name = "Mutability" }
            , { name = "Immutability" }
            ]
      }
    ]


process : List Section -> List PositionedSection
process sections =
    sections
        |> List.Extra.indexedFoldl
            (\sectionPosition section ( lessonPosition, acc ) ->
                let
                    startingLessonPosition : Int
                    startingLessonPosition =
                        if section.resetLessonPosition then
                            1

                        else
                            lessonPosition

                    newLessonPosition : Int
                    newLessonPosition =
                        startingLessonPosition + List.length section.lessons

                    newSection : PositionedSection
                    newSection =
                        { title = section.title
                        , resetLessonPosition = section.resetLessonPosition
                        , position = sectionPosition + 1
                        , lessons =
                            List.indexedMap
                                (\i lesson ->
                                    { name = lesson.name
                                    , position = i + startingLessonPosition
                                    }
                                )
                                section.lessons
                        }
                in
                ( newLessonPosition, newSection :: acc )
            )
            ( 1, [] )
        |> Tuple.second
        |> List.reverse


viewList : (a -> Html msg) -> List a -> Html msg
viewList viewInner list =
    Html.ul [] (List.map (\inner -> Html.li [] [ viewInner inner ]) list)


viewSection : PositionedSection -> Html msg
viewSection s =
    Html.div []
        [ viewDebugDump ( s.position, s.title, s.resetLessonPosition )
        , viewList (\l -> viewDebugDump ( l.position, l.name )) s.lessons
        ]


viewDebugDump : a -> Html msg
viewDebugDump a =
    Html.text <| Debug.toString a

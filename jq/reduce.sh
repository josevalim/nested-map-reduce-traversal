#!/bin/bash

echo \
'[
    {
        "title": "Getting started",
        "reset_lesson_position": false,
        "lessons": [{ "name": "Welcome" }, { "name": "Installation" } ]
    },
    {
        "title": "Basic operator",
        "reset_lesson_position": false,
        "lessons": [{ "name": "Addition / Subtraction" }, { "name": "Multiplication / Division" }]
    },
    {
        "title": "Advanced topics",
        "reset_lesson_position": true,
        "lessons": [{ "name": "Mutability" }, { "name": "Immutability" } ]
    }
]' | \

jq 'reduce .[] as $section (
    [];
    . as $acc |
    ($acc[-1].position | . + 1) as $sec_position |
    $acc + [
            $section + {
                position: $sec_position,
                lessons: ($section.lessons | reduce .[] as $lesson(
                    [];
                    . as $lacc |
                    (if $lacc[-1] == null then (if $section.reset_lesson_position then 1 else $acc[-1].lessons[-1].position + 1 end) else $lacc[-1].position + 1 end)  as $l_position |
                    $lacc + [$lesson + {position: $l_position}]
                    ))
                
            }
        ]
    )'

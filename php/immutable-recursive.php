<?php
const SECTIONS = [
    [
        "title" => "Getting started",
        "reset_lesson_position" => false,
        "lessons" => [
            ["name" => "Welcome"],
            ["name" => "Installation"],
        ],
    ],
    [
        "title" => "Basic operator",
        "reset_lesson_position" => false,
        "lessons" => [
            ["name" => "Addition / Subtraction"],
            ["name" => "Multiplication / Division"],
        ],
    ],
    [
        "title" => "Advanced topics",
        "reset_lesson_position" => true,
        "lessons" => [
            ["name" => "Mutability"],
            ["name" => "Immutability"],
        ],
    ],
];

function parse_recursive(array $sections, int $sectionPosition, int $lessonPosition): array
{
    if (count($sections) === 0) {
        return [];
    }
    $parsedSection = array_merge($sections[0], ["position" => $sectionPosition]);
    if ($parsedSection["reset_lesson_position"]) {
        $lessonPosition = 1;
    }
    foreach ($parsedSection["lessons"] as $idx => $lesson) {
        $parsedSection["lessons"][$idx]["position"] = $lessonPosition++;
    }

    return [$parsedSection, ...parse_recursive(array_slice($sections, 1), $sectionPosition + 1, $lessonPosition)];
}

print_r(parse_recursive(SECTIONS, 1, 1));

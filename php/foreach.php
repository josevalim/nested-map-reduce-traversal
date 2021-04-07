<?php
$sections = [
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

$sectionPosition = 1;
$lessonPosition = 1;

foreach ($sections as $section_idx => $section) {
    $sections[$section_idx]["position"] = $sectionPosition++;
    if ($section["reset_lesson_position"]) {
        $lessonPosition = 1;
    }
    foreach ($section["lessons"] as $lesson_idx => $lesson) {
        $sections[$section_idx]["lessons"][$lesson_idx]["position"] = $lessonPosition++;
    }
}

print_r($sections);


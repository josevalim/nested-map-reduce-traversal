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


  [$sections,,] = array_reduce($sections, function($carry, $section) {
    [$newSections, $sectionPosition, $lessonPosition] = $carry;
    if ($section['reset_lesson_position']) $lessonPosition = 1;

    $section['position'] = $sectionPosition;
    [$section['lessons'], $lessonPosition] = array_reduce($section['lessons'], function($carry, $lesson) {
      [$newLessons, $lessonPosition] = $carry;
      $lesson['position'] = $lessonPosition;
      $newLessons[] = $lesson;
      return [$newLessons, $lessonPosition+1];
    }, [[], $lessonPosition]);

    $newSections[] = $section;
    return [$newSections, $sectionPosition+1, $lessonPosition];
  }, [[], 1, 1]);

  print_r($sections);

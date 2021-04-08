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

  function append_position($item, $counter) {
    $item['position'] = $counter++;
    return [$item, $counter];
  }

  function process_items($items, $depth, $counter) {
    for($i = 0; $i<count($items); $i++) {
      [$item, $counter[$depth]] = append_position(array_shift($items), $counter[$depth]);

      if (!empty($item['lessons'])) {
        if ($item["reset_lesson_position"]) $counter[$depth+1] = 1;
        [$item['lessons'],, $counter] = process_items($item['lessons'], $depth+1, $counter);
      }

      array_push($items, $item);
    }

    return [$items, $depth, $counter];
  }

  [$newSection] = process_items($sections, 0, [1, 1]);
  print_r($newSection);

$data = @"
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
  },

  {
      "title": "For loop",
      "reset_lesson_position": false,
      "lessons": [
          {"name": "For loop"},
          {"name": "Foreach loop"}
      ]
  }
]
"@

$sections = ConvertFrom-Json -InputObject $data
$sectionCounter = 1
$lessonCounter = 1
foreach($section in $sections){
    if($section.reset_lesson_position){
        $lessonCounter=1
    }
    Add-Member -InputObject $section -MemberType NoteProperty -Name "position" -Value $sectionCounter
    foreach($lesson in $section.lessons){
        Add-Member -InputObject $lesson -MemberType NoteProperty -Name "position" -Value $lessonCounter
        $lessonCounter++
    }
    $sectionCounter++
}

Write-Output (ConvertTo-Json -InputObject $sections -Depth 3)
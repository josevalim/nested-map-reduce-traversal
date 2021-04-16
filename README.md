# Traversing nested data-structures

The goal of this repository is to share solutions to a common problem of traversing and annotating data-structures across a variety of programming languages.

*Note: This repository has now been archived with many different solutions across multiple languages, thanks to everyone who contributed!*

## The problem

The algorithm should receive a list of sections. A section is a key-value data structure, with a "title", a "reset_lesson_position" boolean, and a list of "lessons". A lesson is a key-value data structure with the "name" field.

Your job is to traverse the list of sections, adding a position (starting from 1) to each section, and traverse the list of lessons adding a position (starting from 1) to each lesson. Note, however, the lessons position is shared across sections. The lesson position should also be reset if "reset_lesson_position" is true.

Here is an example input (formatted in JSON for convenience):

```json
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
]
```

The output should be (formatted in JSON for convenience):

```json
[
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "position": 1,
    "lessons": [
      {"name": "Welcome", "position": 1},
      {"name": "Installation", "position": 2},
    ]
  },

  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "position": 2,
    "lessons": [
      {"name": "Addition / Subtraction", "position": 3},
      {"name": "Multiplication / Division", "position": 4}
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "position": 3,
    "lessons": [
      {"name": "Mutability", "position": 1},
      {"name": "Immutability", "position": 2}
    ]
  }
]
```

## Sample solution

Here is one way to solve it in Python:

```python
sections = ... # the data from above as Python data structure ellided for convenience

section_counter = 1
lesson_counter = 1

for section in sections:
    if section['reset_lesson_position']:
        lesson_counter = 1

    section['position'] = section_counter
    section_counter += 1

    for lesson in section['lessons']:
        lesson['position'] = lesson_counter
        lesson_counter += 1

print(sections)
```

Thanks to [@nickjanetakis](https://twitter.com/nickjanetakis) for the description of the problem and for contributing this Python solution.

## Contribute

New solutions to the problem are welcome. In order to contribute:

  * Make sure there are no entries for your programming language of choice
  * If there are existing entries, make sure your proposed solution is considerably distinct

For example, avoid new entries that are small variations of existing solutions. Solutions that use different approaches, such as mutability vs immutability, single-pass vs chunking, etc are all welcome though.

Once your solution is ready, please send a pull request. The solution should inside a directory named after the programming language and be a single file named after the approach taken. For example, the Python solution above is placed at:

    python/for-in.py

*If your solution requires more than 1 file, then you can include all of them inside a directory such as `python/for-in/example.py`. However, there is no need to include project setup files.*

It is important the solutions are considered readable and idiomatic. The goal is to focus on readability rather than performance, code golfing, etc.

All code in this repository should be placed in the public domain. Thank you for the time and for sharing a solution!

## License

All code in this repository is in the public domain.

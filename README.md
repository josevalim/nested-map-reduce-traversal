# Traversing nested data-structures

The goal of this repository is to share solutions to a common problem of traversing and annotating data-structures across a variety of programming languages.

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
    "lessons": [
      {"name": "Addition / Subtraction", "position": 3},
      {"name": "Multiplication / Division", "position": 4}
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      {"name": "Mutability", "position": 1},
      {"name": "Immutability", "position": 2}
    ]
  }
]
```

## Sample solution

Here is how the solution would look like in Python:

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

Thanks to @nickjj for the description of the problem and for contributing the Python solution.

## Contribute

Please send a pull request with a solution to the problem for a given programming language in its directory. For example, the Python solution above is placed at:

    python/for-in.py

Multiple entries may be placed for a given programming language, as long as they use different approaches. However, it is important the approaches are considered readable and idiomatic. The goal is to focus on readability rather than performance, code golfing, etc.

All code in this repository should be placed in the public domain.

## License

All code in this repository is in the public domain.

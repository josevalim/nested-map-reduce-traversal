sections = [
    {
        "title": "Getting started",
        "reset_lesson_position": False,
        "lessons": [
            {"name": "Welcome"},
            {"name": "Installation"},
        ],
    },
    {
        "title": "Basic operator",
        "reset_lesson_position": False,
        "lessons": [
            {"name": "Addition / Subtraction"},
            {"name": "Multiplication / Division"},
        ],
    },
    {
        "title": "Advanced topics",
        "reset_lesson_position": True,
        "lessons": [
            {"name": "Mutability"},
            {"name": "Immutability"},
        ],
    },
]

section_counter = 1
lesson_counter = 1

for section in sections:
    if section["reset_lesson_position"]:
        lesson_counter = 1

    section["position"] = section_counter
    section_counter += 1

    for lesson in section["lessons"]:
        lesson["position"] = lesson_counter
        lesson_counter += 1

print(sections)

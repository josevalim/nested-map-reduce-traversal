import json


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


def calculate_lessons(lessons, lesson_position, lesson_counter):
    if len(lessons) == lesson_position:
        return lesson_counter

    lessons[lesson_position]["position"] = lesson_counter

    return calculate_lessons(lessons, lesson_position + 1, lesson_counter + 1)


def calculate(sections, section_counter, lesson_counter):
    if len(sections) == section_counter:
        return

    if sections[section_counter]["reset_lesson_position"]:
        lesson_counter = 1

    sections[section_counter]["position"] = section_counter + 1

    lesson_counter = calculate_lessons(
        sections[section_counter]["lessons"], 0, lesson_counter
    )

    return calculate(sections, section_counter + 1, lesson_counter)


section_counter = 0
lesson_counter = 1

calculate(sections, section_counter, lesson_counter)

print(json.dumps(sections, indent=4, sort_keys=False))

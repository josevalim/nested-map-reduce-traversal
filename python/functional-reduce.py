# This requires Python>=3.8.
# Uses functools.reduce() and does not mutate the input. 


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


from functools import reduce


def in_section(section, sec_pos, les_pos):
    return {
        **section, 'position': sec_pos,
        'lessons': [
            {**lesson, 'position': (last:=pos)}
            for pos, lesson in enumerate(section['lessons'], les_pos)
        ]
    }, sec_pos, last


def cross_section(prev_step, section):
    (sections, sec_pos, les_pos) = prev_step
    new_sec, new_sec_pos, new_les_pos = in_section(
        section, sec_pos+1,
        les_pos+1 if not section['reset_lesson_position'] else 1
    )
    return sections + [new_sec], new_sec_pos, new_les_pos


def calculate(sections):
    init = ([], 0, 0)
    return reduce(cross_section, sections, init)[0]

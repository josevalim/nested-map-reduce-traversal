import json

struct Lesson {
	name string
mut:
	position int
}

struct Section {
	title                 string
	reset_lesson_position bool
mut:
	position int
	lessons  []Lesson
}

fn main() {
	mut sections := [
		Section{'Getting started', false, 0, [Lesson{'Welcome', 0},
			Lesson{'Installation', 0},
		]},
		Section{'Basic operator', false, 0, [Lesson{'Addition / Subtraction', 0},
			Lesson{'Multiplication / Division', 0},
		]},
		Section{'Advanced topics', true, 0, [Lesson{'Mutability', 0},
			Lesson{'Immutability', 0},
		]},
	]

	mut section_counter := 1
	mut lesson_counter := 1

	for i, mut section in sections {
		if section.reset_lesson_position {
			lesson_counter = 1
		}
		sections[i].position = section_counter
		section_counter++

		for j, _ in section.lessons {
			section.lessons[j].position = lesson_counter
			lesson_counter++
		}
	}

	println(json.encode_pretty(sections))
}

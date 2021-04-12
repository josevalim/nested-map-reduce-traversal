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
	mut sections := read_sections()

	mut section_counter, mut lesson_counter := 1, 1
	for mut section in sections {
		section.position = section_counter++

		lesson_counter = reset_if(lesson_counter, section)
		for mut lesson in section.lessons {
			lesson.position = lesson_counter++
		}
	}

	println(json.encode_pretty(sections))
}

fn reset_if(c int, s &Section) int {
	return if s.reset_lesson_position { 1 } else { c }
}

fn read_sections() []Section {
	s := '[{
		"title": "Getting started",
		"reset_lesson_position": false,
		"position":	0,
		"lessons": [{"name": "Welcome", "position": 0}, {"name": "Installation", "position": 0}]
	}, {
		"title": "Basic operator",
		"reset_lesson_position": false,
		"position":	0,
		"lessons": [{"name": "Addition / Subtraction", "position": 0}, {"name": "Multiplication / Division", "position": 0}]
	}, {
		"title": "Advanced topics",
		"reset_lesson_position": true,
		"position":	0,
		"lessons": [{"name": "Mutability", "position": 0}, {"name": "Immutability", "position": 0}]
	}]'
	return json.decode([]Section, s) or {}
}

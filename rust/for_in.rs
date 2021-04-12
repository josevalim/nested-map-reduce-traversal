#[derive(Default, Debug)]
struct Lesson {
    name: &'static str,
    position: Option<u64>,
}

#[derive(Default, Debug)]
struct Section<'a> {
    title: &'static str,
    reset_lesson_position: bool,
    position: Option<u64>,
    lessons: &'a mut [Lesson],
}

fn apply_positions(input: &mut [Section]) {
    let mut lesson_counter = 1;
    let mut section_counter = 1;

    for section in input {
        if section.reset_lesson_position {
            lesson_counter = 1;
        }

        section.position = Some(section_counter);
        section_counter += 1;

        for lesson in section.lessons.iter_mut() {
            lesson.position = Some(lesson_counter);
            lesson_counter += 1;
        }
    }
}

fn main() {
    let mut input = [
        Section {
            title: "Getting started",
            reset_lesson_position: false,
            position: None,
            lessons: &mut [
                Lesson {
                    name: "Welcome",
                    position: None,
                },
                Lesson {
                    name: "Installation",
                    position: None,
                },
            ],
        },
        Section {
            title: "Basic operator",
            reset_lesson_position: false,
            position: None,
            lessons: &mut [
                Lesson {
                    name: "Addition / Subtraction",
                    position: None,
                },
                Lesson {
                    name: "Multiplication / Division",
                    position: None,
                },
            ],
        },
        Section {
            title: "Advanced topics",
            reset_lesson_position: true,
            position: None,
            lessons: &mut [
                Lesson {
                    name: "Mutability",
                    position: None,
                },
                Lesson {
                    name: "Immutability",
                    position: None,
                },
            ],
        },
    ];

    apply_positions(&mut input);

    println!("finished: {:#?}", input);
}

struct Lesson {
    name: String,
    position: Option<u32>,
}

struct Section {
    title: String,
    reset_position_title: bool,
    lessons: Vec<Lesson>,
    position: Option<u32>,
}

fn main() {
    let sections: Vec<Section> = vec![
        Section {
            title: String::from("Getting started"),
            reset_position_title: false,
            lessons: vec![
                Lesson {
                    name: String::from("Welcome"),
                    position: None,
                },
                Lesson {
                    name: String::from("Installation"),
                    position: None,
                }
            ],
            position: None,
        },
        Section {
            title: String::from("Basic operator"),
            reset_position_title: false,
            lessons: vec![
                Lesson {
                    name: String::from("Addition / Subtraction"),
                    position: None,
                },
                Lesson {
                    name: String::from("Multiplication / Division"),
                    position: None,
                },
            ],
            position: None,
        },
        Section {
            title: String::from("Advanced topics"),
            reset_position_title: true,
            lessons: vec![
                Lesson {
                    name: String::from("Mutability"),
                    position: None,
                },
                Lesson {
                    name: String::from("Immutability"),
                    position: None,
                }
            ],
            position: None,
        }
    ];
    
    let mut section_position = 0;
    let mut lesson_position = 0;

    let formatted_sections: Vec<Section> = sections.iter().map(|s| {
        if s.reset_position_title == true {
            lesson_position = 0;
        }

        section_position += 1;

        Section {
            lessons: s.lessons.iter().map(|l| {
                lesson_position += 1;

                Lesson {
                    position: Some(lesson_position),
                    name: l.name.clone(),
                    ..*l
                }
            }).collect(),
            title: s.title.clone(),
            position: Some(section_position),
            ..*s
        }
    }).collect();
}


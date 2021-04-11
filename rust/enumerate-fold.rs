#[derive(Debug, Clone)]
struct Lesson {
    name: String,
    position: Option<u32>,
}

#[derive(Debug, Clone)]
struct Section {
    title: String,
    reset_position_title: bool,
    lessons: Vec<Lesson>,
    position: Option<usize>,
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
                },
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
                },
            ],
            position: None,
        },
    ];

    let formatted_sections =
        sections
            .iter()
            .enumerate()
            .try_fold(Vec::<Section>::new(), |mut acc, (idx, s)| {
                let lesson_position = if s.reset_position_title || acc.is_empty() {
                    1
                } else {
                    acc.last()?.lessons.last()?.position? + 1
                };

                acc.push(Section {
                    lessons: s
                        .lessons
                        .iter()
                        .zip(lesson_position..)
                        .map(|(l, p)| Lesson {
                            position: Some(p),
                            name: l.name.clone(),
                            ..*l
                        })
                        .collect(),
                    title: s.title.clone(),
                    position: Some(idx + 1),
                    ..*s
                });

                Some(acc)
            });

    println!("{:?}", formatted_sections.unwrap());
}

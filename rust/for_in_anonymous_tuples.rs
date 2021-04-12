fn main() {
    let mut input = [
        ("Getting started", false, None, &mut [
            ("Welcome", None),
            ("Installation", None),
        ]),
        ("Basic operator", false, None, &mut [
            ("Addition / Subtraction", None),
            ("Multiplication / Division", None),
        ]),
        ("Advanced topics", true, None, &mut [
            ("Mutability", None),
            ("Immutability", None)
        ]),
    ];

    let mut lesson_counter = 1;
    let mut section_counter = 1;

    for section in &mut input {
        if section.1 {
            lesson_counter = 1;
        }

        section.2 = Some(section_counter);
        section_counter += 1;

        for lesson in section.3.iter_mut() {
            lesson.1 = Some(lesson_counter);
            lesson_counter += 1;
        }
    }

    println!("{:#?}", input);
}

// npx -p rescript bsc array.res | node
type lession = {
  name: string,
  position: option<int>,
}

type section = {
  title: string,
  reset_lesson_position: bool,
  position: option<int>,
  lessons: array<lession>,
}

let sections: array<section> = [
  {
    title: "Getting started",
    reset_lesson_position: false,
    position: None,
    lessons: [{name: "Welcome", position: None}, {name: "Installation", position: None}],
  },
  {
    title: "Basic operator",
    reset_lesson_position: false,
    position: None,
    lessons: [
      {name: "Addition / Subtraction", position: None},
      {name: "Multiplication / Division", position: None},
    ],
  },
  {
    title: "Advanced topics",
    reset_lesson_position: true,
    position: None,
    lessons: [{name: "Mutability", position: None}, {name: "Immutability", position: None}],
  },
]

let (sections, _, _) = sections->Js.Array2.reduce((acc, section) => {
  let (sections, sectionCounter, lessonCounter) = acc
  let lessonCounter = section.reset_lesson_position ? 1 : lessonCounter

  let (lessons, lessonCounter) = section.lessons->Js.Array2.reduce((acc, lesson) => {
    let (lessons, lessonCounter) = acc
    let _ = lessons->Js.Array2.push({
      ...lesson,
      position: Some(lessonCounter),
    })

    (lessons, lessonCounter + 1)
  }, ([], lessonCounter))

  let _ = sections->Js.Array2.push({
    ...section,
    position: Some(sectionCounter),
    lessons: lessons,
  })

  (sections, sectionCounter + 1, lessonCounter)
}, ([], 1, 1))

Js.log2("%o", sections)

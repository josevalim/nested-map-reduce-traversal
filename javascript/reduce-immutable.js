const sections = [
  {
    title: 'Getting started',
    reset_lesson_position: false,
    lessons: [{ name: 'Welcome' }, { name: 'Installation' }],
  },
  {
    title: 'Basic operator',
    reset_lesson_position: false,
    lessons: [
      { name: 'Addition / Subtraction' },
      { name: 'Multiplication / Division' },
    ],
  },
  {
    title: 'Advanced topics',
    reset_lesson_position: true,
    lessons: [{ name: 'Mutability' }, { name: 'Immutability' }],
  },
]

const lessonReducer = (({lessonCounter, result}, lesson) => {
  const updatedLesson = {
    position: lessonCounter + 1,
    ...lesson
  }
  return {lessonCounter: lessonCounter + 1, result: [...result, updatedLesson]}
})

const sectionReducer = ({lessonCounter, result}, section, index) => {
  const { reset_lesson_position, lessons } = section
  const nextLessonCounter = reset_lesson_position ? 0 : lessonCounter
  const updatedLessons = lessons.reduce(
    lessonReducer,
    {lessonCounter: nextLessonCounter, result: []}
  )
  const updatedSection = {
    ...section,
    lessons: updatedLessons.result,
    position: index + 1
  }

  return {
    result: [...result, updatedSection],
    lessonCounter: updatedLessons.lessonCounter
  }
}

const { result } = sections.reduce(sectionReducer, {lessonCounter: 0, result: []})

console.log(JSON.stringify(result, null, 2))

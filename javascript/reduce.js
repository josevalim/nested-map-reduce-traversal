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

let lessonCounter = 1
let sectionCounter = 1

sections.reduce((sectionsCache, section) => {
  // Reset the lessonCounter if evaluates to true.
  if (section.reset_lesson_position) {
    lessonCounter = 1
  }
  // Set the section.position and increment the sectionCounter.
  section['position'] = sectionCounter
  sectionCounter += 1

  // Reduce over the "lessons" setting the position
  // and incrementing the lessonCounter
  section.lessons.reduce((lessonsCache, lesson) => {
    lesson['position'] = lessonCounter
    lessonCounter += 1

    lessonsCache.push(lesson)
    return lessonsCache
  }, [])

  sectionsCache.push(section)
  return sectionsCache
}, [])

console.log(sections)

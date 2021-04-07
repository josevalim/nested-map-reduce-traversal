const util = require('util')

Object.defineProperty(Array.prototype, 'mapWithState', {
  value: function (state, fn, results = []) {
    const collection = this
    if (collection.length >= 1) {
      const [nextElement, ...rest] = collection
      const [newResult, nextState] = fn(nextElement, state)
      return rest.mapWithState(nextState, fn, [...results, newResult])
    } else {
      return [results, state]
    }
  }
})

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

const [res, _state] = sections.mapWithState( { lessonCounter: 1, sectionCounter: 1 }, (section, { lessonCounter, sectionCounter }) => {
  const newLessonCounter = section.reset_lesson_position ? 1 : lessonCounter
  const [lessons, { lessonCounter: counter }] = section.lessons.mapWithState( { lessonCounter: newLessonCounter }, (lesson, { lessonCounter }) => (
    [
      {
        ...lesson,
        position: lessonCounter
      },
      {
        lessonCounter: lessonCounter + 1
      }
    ])
  )
  return [
    {
      ...section,
      lessons,
      position: sectionCounter
    },
    {
      lessonCounter: counter,
      sectionCounter: sectionCounter + 1
    }
  ]
})

console.log(util.inspect(res, {showHidden: false, depth: null}))

const util = require('util')

Object.defineProperty(Array.prototype, 'mapWithStateAction', {
  value: function (state, stateAction, mapFn, results = []) {
    const collection = this
    if (collection.length >= 1) {
      const [nextElement, ...rest] = collection
      const newResult = mapFn(nextElement, state)
      const nextState = stateAction(state)
      return rest.mapWithStateAction(nextState, stateAction, mapFn, [...results, newResult])
    } else {
      return results
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

const incrementSectionCounter = ({ sectionCounter, ...rest }) => ({
  ...rest,
  sectionCounter: sectionCounter + 1
})

const incrementSharedLessonCounter = ({ lessonCounterState, ...rest }) => {
  lessonCounterState.update(lessonCounterState.get() + 1)
  return {
  ...rest,
    lessonCounterState
  }
}

const sharedState = (initialState) => {
  let state = initialState

  const get = () => state

  const update = (value) => {
    state = value
  }

  return {
    get,
    update
  }
}

const lessonCounterState = sharedState(1)

const result = sections.mapWithStateAction({ lessonCounterState, sectionCounter: 1 }, incrementSectionCounter, (section, { lessonCounterState, sectionCounter }) => {
  if(section.reset_lesson_position) {
    lessonCounterState.update(1)
  }
  const lessons = section.lessons.mapWithStateAction({ lessonCounterState }, incrementSharedLessonCounter, (lesson, { lessonCounterState }) => (
    {
      ...lesson,
      position: lessonCounterState.get()
    })
  )
  return {
    ...section,
    lessons,
    position: sectionCounter
  }
})

console.log(util.inspect(result, {showHidden: false, depth: null}))

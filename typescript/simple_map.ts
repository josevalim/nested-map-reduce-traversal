const sections = [
  {
    title: "Getting started",
    reset_lesson_position: false,
    lessons: [{ name: "Welcome" }, { name: "Installation" }],
  },

  {
    title: "Basic operator",
    reset_lesson_position: false,
    lessons: [
      { name: "Addition / Subtraction" },
      { name: "Multiplication / Division" },
    ],
  },

  {
    title: "Advanced topics",
    reset_lesson_position: true,
    lessons: [{ name: "Mutability" }, { name: "Immutability" }],
  },
];

type Lesson = { name: string };
type Section = {
  title: string,
  reset_lesson_position: boolean,
  lessons: Lesson[],
};

function addPosition(sections: Section[]) {
  let lessonCounter = 1;
  return sections.map((section, idx) => {
    if (section.reset_lesson_position) lessonCounter = 1;
    return {
      ...section,
      position: idx + 1,
      lessons: section.lessons.map((lesson) => ({
        ...lesson,
        position: lessonCounter++,
      })),
    };
  });
}

console.log(addPosition(sections));

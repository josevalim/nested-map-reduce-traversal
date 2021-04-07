interface Section {
  title: string;
  reset_lesson_position: boolean;
  lessons: { name: string }[];
}

interface SingleLesson {
  title: string;
  reset_lesson_position: boolean;
  lesson: { name: string };
  index: number;
}

interface SingleLessonWithIndex extends SingleLesson {
  index: number;
}

interface SectionWithIndex extends Section {
  index: number;
}

const sections: Section[] = [
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

/**
 * Splits the sections into a nested array with reset_lesson_position as the break point
 *
 */
function splitSections(sections: Section[]): Section[][] {
  return sections.reduce((acc, curr): Section[][] => {
    if (acc.length === 0) {
      return [[curr]];
    }
    if (curr.reset_lesson_position) {
      return [...acc, [curr]];
    }
    const currentArray = acc[acc.length - 1];
    const rest = acc.slice(0, acc.length - 1);
    return [...rest, [...currentArray, curr]];
  }, [] as Section[][]);
}

/**
 * Turns an array of sections into a flattened array of lessons with parent section-data added
 * Adds an index to track which section the lessons belong to
 */

function flattenLessons(sections: Section[]): SingleLesson[] {
  return sections.reduce((acc, curr, index) => {
    const lessons = curr.lessons.map((lesson) => ({
      title: curr.title,
      reset_lesson_position: curr.reset_lesson_position,
      index,
      lesson,
    }));
    return [...acc, ...lessons];
  }, [] as SingleLesson[]);
}

/**
 * Adds a position element to each lesson, counting from 1 to end of the array
 */

function indexLessons(lessons: SingleLesson[]): SingleLessonWithIndex[] {
  return lessons.map((section, index) => ({
    ...section,
    lesson: {
      ...section.lesson,
      position: index + 1,
    },
  }));
}

function buildSection(lesson: SingleLessonWithIndex) {
  return {
    title: lesson.title,
    reset_lesson_position: lesson.reset_lesson_position,
    lessons: [lesson.lesson],
    index: lesson.index,
  };
}

/**
 * Turns an array of split-up lessons into an array of Sections, using the index on each lesson to group them into sections
 *
 */
function reconstituteSections(lessons: SingleLessonWithIndex[]): Section[] {
  const reconstitutedlessons = lessons.reduce((acc, curr) => {
    if (acc.length === 0) {
      return [buildSection(curr)];
    }
    const currentSection = acc[acc.length - 1];
    const rest = acc.slice(0, length - 1);
    if (curr.index === currentSection.index) {
      return [
        ...rest,
        {
          ...currentSection,
          lessons: [...currentSection.lessons, curr.lesson],
        },
      ];
    }
    return [...acc, buildSection(curr)];
  }, [] as SectionWithIndex[]);
  //remove the index for neatness
  return reconstitutedlessons.map(({ index, ...section }) => ({ ...section }));
}

/**
 * Split the sections into a nested array of sections, flatten each section's lessons, index them, reconstute them into a nested array of sections, and then flatten again.
 */

const result = splitSections(sections)
  .map((section) => reconstituteSections(indexLessons(flattenLessons(section))))
  .reduce((acc, curr) => [...acc, ...curr]);

console.log("result", result);

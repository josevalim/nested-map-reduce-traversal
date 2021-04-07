interface Section {
  title: string;
  reset_lesson_position: boolean;
  lessons: { name: string }[];
}

interface SectionWithPosition extends Section {
  lessons: {
    name: string;
    position: number;
  }[];
}

const input: Section[] = [
  {
    title: "Getting started",
    reset_lesson_position: false,
    lessons: [{ name: "Welcome" }, { name: "Installation" }]
  },
  {
    title: "Basic operator",
    reset_lesson_position: false,
    lessons: [
      { name: "Addition / Subtraction" },
      { name: "Multiplication / Division" }
    ]
  },
  {
    title: "Advanced topics",
    reset_lesson_position: true,
    lessons: [{ name: "Mutability" }, { name: "Immutability" }]
  }
];

/**
 * Splits the sections into a nested array with reset_lesson_position as the break point
 *
 */
function splitSections(sections: Section[]): Section[][] {
  return sections.reduce(
    (acc, curr): Section[][] => {
      if (acc.length === 0) {
        return [[curr]];
      }
      if (curr.reset_lesson_position) {
        return [...acc, [curr]];
      }
      const currentArray = acc[acc.length - 1];
      const rest = acc.slice(0, acc.length - 1);
      return [...rest, [...currentArray, curr]];
    },
    [] as Section[][]
  );
}

/**
 * Adds a position to each lesson based on a running counter
 */

function addPosition(input: Section[]): SectionWithPosition[] {
  const result = input.reduce(
    (
      {
        position,
        sections
      }: { position: number; sections: SectionWithPosition[] },
      section
    ) => ({
      position: position + section.lessons.length,
      sections: [
        ...sections,
        {
          ...section,
          lessons: section.lessons.map((lesson, lessonIndex) => ({
            ...lesson,
            position: position + lessonIndex
          }))
        }
      ]
    }),
    { position: 1, sections: [] as SectionWithPosition[] }
  );

  return result.sections;
}

const result = splitSections(input)
  .map(sections => addPosition(sections))
  .reduce((acc, curr) => [...acc, ...curr]);

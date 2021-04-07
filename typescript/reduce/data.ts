export default [
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
] as Data[];

export interface Data {
  title: string;
  reset_lesson_position: boolean;
  postion?: number;
  lessons: Array<{ name: "Mutability"; position?: number }>;
}

import data, { Data } from './data.ts';

const ouptut = data.reduce(
  (acc, cur, i) => {
    const startPos = cur.reset_lesson_position ? 0 : acc.lastLesson;
    const nS = {
      ...cur,
      position: i + 1,
      lessons: cur.lessons.map((l, lI) => ({
        ...l,
        position: startPos + lI + 1,
      })),
    };
    acc.result.push(nS);
    acc.lastLesson = startPos + cur.lessons.length;
    return acc;
  },
  { result: [] as Data[], lastLesson: 0 }
).result;

console.log(ouptut);

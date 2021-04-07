import {
    comp,
    iterator,
    map,
    mapcat,
    mapIndexed,
    partitionWhen,
} from "@thi.ng/transducers";

interface Section {
    title: string;
    reset_lesson_position?: boolean;
    lessons: { name: string; position?: number }[];
    position?: number;
}

/**
 * Source data
 */
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
 * Helper function to immutably set `key` in given `obj`ect.
 *
 * @param obj
 * @param key
 * @param val
 */
const setKey = <T, K extends keyof T>(obj: T, key: keyof T, val: T[K]) => ({
    ...obj,
    [key]: val,
});

const transformed = [
    ...iterator(
        // build a composed multi-stage transformation function (transducer)
        comp(
            // group sections until next reset flag
            partitionWhen((section) => !!section.reset_lesson_position),
            // transform each group of sections w/ its own internal position counter
            // however `mapcat` (unlike `map`) yields a flat sequence of results
            mapcat((group) => {
                // lesson-within-group counter
                let offset = 1;
                return map((section) => {
                    const pos = offset;
                    offset += section.lessons.length;
                    // immutably label lessons
                    return setKey(
                        section,
                        "lessons",
                        section.lessons.map((l, i) =>
                            setKey(l, "position", pos + i)
                        )
                    );
                }, group);
            }),
            // label/number sections themselves (start from #1)
            mapIndexed((i, section) => setKey(section, "position", i), 1)
        ),
        sections
    ),
];

console.log(JSON.stringify(transformed, null, 4));

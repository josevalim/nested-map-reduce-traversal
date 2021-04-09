using System;
using System.Linq;
using System.Text.Json;

var data = new Section[]
{
    new("Getting started", false, new Lesson[]
    {
        new("Welcome"),
        new("Installation"),
    }),
    new("Basic operator", false, new Lesson[]
    {
        new("Addition / Subtraction"),
        new("Multiplication / Division"),
    }),
    new("Advanced topics", true, new Lesson[]
    {
        new("Mutability"),
        new("Immutability"),
    }),
};
var result = Recursive(data, null);

Console.WriteLine(JsonSerializer.Serialize(result, new() { WriteIndented = true, }));

Section[] Recursive(Span<Section> sections, Section? previousSection)
{
    if (sections.Length == 0)
    {
        return new Section[0];
    }

    var currentSection = sections[0];
    var newSection = currentSection with
    {
        position = 1 + (previousSection?.position ?? 0),
        lessons = currentSection.lessons.Select((lesson, i) => lesson with
        {
            position = 1 + i + (currentSection.reset_lesson_position ? 0 : previousSection?.lessons[^1].position ?? 0),
        }).ToArray(),
    };

    return new[] { newSection }.Concat(Recursive(sections[1..], newSection)).ToArray();
}

record Lesson(string name, int? position = null);
record Section(string title, bool reset_lesson_position, Lesson[] lessons, int? position = null);

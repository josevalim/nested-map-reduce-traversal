using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;

List<InputSection> input = new();
input.Add(new InputSection("Getting started", false, new List<InputLesson>() { new InputLesson("Welcome"), new InputLesson("Installation") }));
input.Add(new InputSection("Basic operator", false, new List<InputLesson>() { new InputLesson("Addition / Subtraction"), new InputLesson("Multiplication / Division") }));
input.Add(new InputSection("Advanced topics", true, new List<InputLesson>() { new InputLesson("Mutability"), new InputLesson("Immutability") }));

int sectionCount = 1;
int lessonCount = 1;
var output = input.Select(section =>
{
    if (section.reset_lesson_position)
    {
        lessonCount = 1;
    }

    var outputLessons = section.lessons.Select(e => new OutputLesson(e.name, lessonCount++)).ToList();

    return new OutputSection(section.title, section.reset_lesson_position, sectionCount++, outputLessons);
}).ToList();

Console.WriteLine(JsonSerializer.Serialize(output));

record InputLesson(string name);
record OutputLesson(string name, int position);
record InputSection(string title, bool reset_lesson_position, List<InputLesson> lessons);
record OutputSection(string title, bool reset_lesson_position, int position, List<OutputLesson> lessons);

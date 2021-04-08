#include "for.h"

/*
  I'm certain I've comitted all kinds of C sins with this.
  Relying on null terminated strings that don't exceed 254 character (need 1 for nulls)
*/

int main(int argc, char** argv)
{
  const int sections_size = 3;
  struct Section sections[] = {
    {
      .title = "Getting started",
      .reset_lesson_position = 0,
      .lessons_size = 2,
      .lessons = {
        {.name = "Welcome"},
        {.name = "Installation"}
      }
    },
    {
      .title = "Basic operator",
      .reset_lesson_position = 0,
      .lessons_size = 2,
      .lessons = {
        {.name = "Addition / Subtraction"},
        {.name = "Multiplication / Division"}
      }
    },
    {
      .title = "Advanced topics",
      .reset_lesson_position = 1,
      .lessons_size = 2,
      .lessons = {
        {.name = "Mutability"},
        {.name = "Immutability"}
      }
    }
  };

  int lesson_position = 1;
  for (int i = 0; i < sections_size; i++)
  {
    struct Section* section = &sections[i];
    section->position = i + 1;
    if (section->reset_lesson_position)
    {
      lesson_position = 1;
    }

    for (int li = 0; li < section->lessons_size; li++)
    {
      struct Lesson* lesson = &section->lessons[li];
      lesson->position = lesson_position;
      lesson_position += 1;
    }
  }

  display_sections(sections_size, sections);

  return 0;
}

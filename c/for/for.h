#ifndef FOR_H
#define FOR_H

#include <stdio.h>

struct Lesson {
  int position;
  char name[255];
};

struct Section {
  int position;
  char title[255];
  int reset_lesson_position;
  int lessons_size;
  /* For now, every section will have at most 2 lessons, the count above is  the
     actual number of lessons present.
     I would normally do a larger power of two, maybe even 16 */
  struct Lesson lessons[2];
};

static void display_sections(int sections_size, struct Section* sections)
{
  const int last_section_index = sections_size - 1;

  printf("[\n");
  for (int i = 0; i < sections_size; i++)
  {
    struct Section* section = &sections[i];
    printf("\t{\n\t\t\"title\": \"%s\",\n\t\t\"reset_lesson_position\": %s,\n\t\t\"position\": %i,\n",
            section->title,
            section->reset_lesson_position ? "true" : "false",
            section->position);
    printf("\t\t\"lessons\": [\n");
    const int last_lesson_index = section->lessons_size - 1;
    for (int li = 0; li < section->lessons_size; li++)
    {
      struct Lesson* lesson = &section->lessons[li];
      printf("\t\t\t{\n");
      printf("\t\t\t\t\"name\": \"%s\",\n", lesson->name);
      printf("\t\t\t\t\"position\": %i\n", lesson->position);
      printf("\t\t\t}%s\n", li == last_lesson_index ? "" : ",");
    }
    printf("\t\t]\n");
    printf("\t}%s\n", last_section_index == i ? "" : ",");
  }
  printf("]\n");
}
#endif

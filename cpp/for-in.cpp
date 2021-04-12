// build: g++ for-in.cpp -o for-in -std=c++11

#include <map>
#include <string>
#include <vector>

struct Lesson
{
  Lesson(std::string name)
    : name(name) {}

  std::string name;
  unsigned int position{0};
};

using Lessons = std::vector<Lesson>;

struct Section
{
  Section(std::string title, bool reset, Lessons lessons)
    : title(title), reset_lesson_position(reset), lessons(lessons) {}

  std::string title;
  bool reset_lesson_position;
  unsigned int position{0};
  Lessons lessons;
};

using Sections = std::vector<Section>;

int main()
{
  auto sections = Sections{
    {"Getting started", false,
     {
       {"Welcome"},
       {"Installation"}
     }
    },
    {"Basic operator", false,
     {
       {"Addition / Subtraction"},
       {"Multiplication / Division"}
     }
    },
    {"Advanced topics", true,
     {
       {"Mutability"},
       {"Immutability"}
     }
    }
  };

  unsigned int section_counter = 1;
  unsigned int lesson_counter = 1;

  for (auto& section : sections)
    {
      if (section.reset_lesson_position)
        lesson_counter = 1;

      section.position = section_counter++;

      for (auto& lesson : section.lessons)
          lesson.position = lesson_counter++;
    }

  return 0;
}

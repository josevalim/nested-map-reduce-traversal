// build: g++ recursive.cpp -o recursive -std=c++11

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

int calculateLessons(Lessons lessons, unsigned int lesson_position, unsigned int lesson_counter){
    if (lessons.size() == lesson_position)
        return lesson_counter;

    lessons[lesson_position].position = lesson_counter;

    return calculateLessons(lessons, lesson_position+1, lesson_counter+1);
}

void calculate(Sections sections, unsigned int section_counter, unsigned int lesson_counter){
    if (sections.size() == section_counter)
        return;
    
    if (sections[section_counter].reset_lesson_position)
        lesson_counter = 1;
    
    sections[section_counter].position = section_counter + 1;

    lesson_counter = calculateLessons(sections[section_counter].lessons, 0, lesson_counter);

    return calculate(sections, section_counter+1, lesson_counter);
}

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

    calculate(sections, section_counter, lesson_counter);

    return 0;
}

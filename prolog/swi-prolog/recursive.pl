% SWI-Prolog solution - pure recursive approach with dictionaries
% tested on version 8.3.18

sections([
    section{
        title: "Getting started", 
        reset_lesson_position: false,
        lessons: [ 
            lesson{name: "Welcome"}, 
            lesson{name: "Installation"} ] 
    },
    section{
        title: "Basic operator", 
        reset_lesson_position: false,
        lessons: [ 
            lesson{name: "Addition / Subtraction"}, 
            lesson{name: "Multiplication / Division"} ] 
    }, 
    section{
        title: "Advanced topics", 
        reset_lesson_position: true,
        lessons: [ 
            lesson{name: "Mutability"}, 
            lesson{name: "Immutability"} 
        ] 
    }
]).

add_index(Sections, IndexedSections) :-
    add_section_index(Sections, IndexedSections, 1, 1).

add_section_index([], [], _, _).
add_section_index([Section | Rest], [IndexedSection | IndexedRest], SectionIndex, LessonIndex) :-
    put_dict(position, Section, SectionIndex, HalfIndexedSection),
    NextSectionIndex is SectionIndex + 1,
    reset_lesson_index(Section, LessonIndex, UpdatedLessonIndex),
    add_lesson_index(Section.lessons, IndexedLessons, UpdatedLessonIndex, NextLessonIndex),
    put_dict(lessons, HalfIndexedSection, IndexedLessons, IndexedSection),
    add_section_index(Rest, IndexedRest, NextSectionIndex, NextLessonIndex).
    
add_lesson_index([], [], Index, Index).
add_lesson_index([Lesson | Rest], [IndexedLesson | IndexedRest], LessonIndex, NextIndex) :-
    put_dict(position, Lesson, LessonIndex, IndexedLesson),
    NewLessonIndex is LessonIndex + 1,
    add_lesson_index(Rest, IndexedRest, NewLessonIndex, NextIndex).
    
reset_lesson_index(Section, _, 1) :-
    get_dict(reset_lesson_position, Section, true), !.
reset_lesson_index(_, CurrentIndex, CurrentIndex).
    
:- sections(Sections), add_index(Sections, IndexedSections), writeln(IndexedSections).
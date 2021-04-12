sections = struct (
  "title", {"Getting started", "Basic operator", "Advanced topics"},
  "reset_lesson_position", {false, false, true},
  "lessons", {
    struct("name", {"Welcome", "Installation"}),
    struct("name", {"Addition / Subtraction", "Multiplication / Division"}),
    struct("name", {"Mutability", "Immutability"})}');

section_counter = 1;
lesson_counter  = 1;

for i=1:numel (sections)
  sections(i).position = section_counter;
  section_counter++;

  if sections(i).reset_lesson_position
    lesson_counter = 1;
  endif

  for j=1:numel (sections(i).lessons)
    sections(i).lessons(j).position = lesson_counter;
    lesson_counter++;
  endfor
endfor

disp (sections);

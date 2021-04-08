(* This approach is different from the other OCaml one in using OCaml's
   objects, which are 'statically duck-typed', immutable data
   structures. The unique thing about objects is that the output
   sections and lessons, with annotated positions, are considered by the
   compiler to be subtypes of the input sections and lessons. So any
   code that works with the latter will work with the former. This is
   something missing from other FP languages which don't have good
   support for subtyping. *)

module Input = struct
  let lesson name = object
    method name = name
  end

  let section ~title ~reset_lesson_position lessons = object
    method title = title
    method reset_lesson_position = reset_lesson_position
    method lessons = lessons
  end

  let sections = [
    section ~title:"Getting started" ~reset_lesson_position:false [
      lesson "Welcome";
      lesson "Installation";
    ];
    section ~title:"Basic operator" ~reset_lesson_position:false [
      lesson "Addition / Subtraction";
      lesson "Multiplication / Division";
    ];
    section ~title:"Advanced topics" ~reset_lesson_position:true [
      lesson "Mutability";
      lesson "Immutability";
    ];
  ]
end

type positions = { section : int; lesson : int }

let add_lesson_position (curr_positions, curr_lessons) lesson =
  let new_lesson = object
    method name = lesson#name
    method position = curr_positions.lesson
  end
  in
  let curr_positions =
    { curr_positions with lesson = succ curr_positions.lesson }
  in
  curr_positions, new_lesson :: curr_lessons

let add_section_position (curr_positions, curr_sections) section =
  let curr_positions =
    if section#reset_lesson_position then { curr_positions with lesson = 1 }
    else curr_positions
  in
  let initial = curr_positions, [] in
  let curr_positions, lessons =
    List.fold_left add_lesson_position initial section#lessons
  in
  let new_section = object
    method title = section#title
    method reset_lesson_position = section#reset_lesson_position
    method position = curr_positions.section
    method lessons = List.rev lessons
  end
  in
  let curr_positions =
    { curr_positions with section = succ curr_positions.section }
  in
  curr_positions, new_section :: curr_sections

let initial = { section = 1; lesson = 1 }, []

let _, sections = List.fold_left add_section_position initial Input.sections
let sections = List.rev sections

(* Print out and check results: *)

let print_lesson lesson =
  Printf.printf
    {|    - name: %s
      position: %d
|}
    lesson#name
    lesson#position

let print_section section =
  Printf.printf
    {|- title: %s
  reset_lesson_position: %b
  position: %d
  lessons:
|}
    section#title
    section#reset_lesson_position
    section#position;
  List.iter print_lesson section#lessons

let () = List.iter print_section sections


module Lesson = struct
  type t = { name : string; position : int option }
end

module Chapter = struct
  type t = {
    title : string;
    reset_lesson_position : bool;
    lessons : Lesson.t list;
    position : int option;
  }
end

let input : Chapter.t list =
  [
    {
      title = "Getting started";
      reset_lesson_position = false;
      lessons =
        [
          { name = "Welcome"; position = None };
          { name = "Installation"; position = None };
        ];
      position = None;
    };
    {
      title = "Basic operator";
      reset_lesson_position = false;
      lessons =
        [
          { name = "Addition / Subtraction"; position = None };
          { name = "Multiplication / Division"; position = None };
        ];
      position = None;
    };
    {
      title = "Advanced topics";
      reset_lesson_position = true;
      lessons =
        [
          { name = "Mutability"; position = None };
          { name = "Immutability"; position = None };
        ];
      position = None;
    };
  ]

(* These functions are tail-call optimized so super efficient *)
let rec process_lessons ?(i = 1) ?(lessons = []) = function
  | [] -> lessons
  | lesson :: rest_lessons ->
      process_lessons ~i:(i + 1)
        ~lessons:({ lesson with Lesson.position = Some i } :: lessons)
        rest_lessons

and process_chapters ?(i = 1) ?(lesson_i = 1) ?(chapters = []) = function
  | [] -> List.rev chapters
  | (chapter : Chapter.t) :: rest_chapters ->
      let lessons = process_lessons ~i:lesson_i chapter.lessons in
      let lesson_i : int =
        match (chapter, lessons) with
        | { reset_lesson_position = true }, _ -> 1
        | _, { position = Some pos } :: _ -> pos
        (* we need to take care of this case, because we reuse the type, even
           though the position is never None *)
        | _, { position = None } :: _ | _, [] -> lesson_i
      in
      process_chapters
        ~chapters:
          ({ chapter with position = Some i; lessons = List.rev lessons }
           :: chapters)
        ~i:(i + 1) ~lesson_i rest_chapters

(* the end result *)
let chapters = process_chapters input

module Lesson = struct
  type t = { name : string }
end

module PositionedLesson = struct
  type t = { name : string; position : int }
end

module Chapter = struct
  type t = {
    title : string;
    reset_lesson_position : bool;
    lessons : Lesson.t list;
  }
end

module PositionedChapter = struct
  type t = {
    title : string;
    reset_lesson_position : bool;
    lessons : PositionedLesson.t list;
    position : int;
  }
end

let input : Chapter.t list =
  [
    {
      title = "Getting started";
      reset_lesson_position = false;
      lessons = [ { name = "Welcome" }; { name = "Installation" } ];
    };
    {
      title = "Basic operator";
      reset_lesson_position = false;
      lessons =
        [
          { name = "Addition / Subtraction" };
          { name = "Multiplication / Division" };
        ];
    };
    {
      title = "Advanced topics";
      reset_lesson_position = true;
      lessons = [ { name = "Mutability" }; { name = "Immutability" } ];
    };
  ]

(* These functions are tail-call optimized so super efficient *)
let rec process_lessons ?(i = 1) ?(lessons : PositionedLesson.t list = []) =
  function
  | [] -> lessons
  | (lesson : Lesson.t) :: rest_lessons ->
      process_lessons ~i:(i + 1)
        ~lessons:({ name = lesson.name; position = i } :: lessons)
        rest_lessons

and process_chapters ?(i = 1) ?(lesson_i = 1)
    ?(chapters : PositionedChapter.t list = []) = function
  | [] -> List.rev chapters
  | (chapter : Chapter.t) :: rest_chapters ->
      let lessons = process_lessons ~i:lesson_i chapter.lessons in
      let lesson_i : int =
        match (chapter, lessons) with
        | { reset_lesson_position = true }, _ -> 1
        | _, { position = pos } :: _ -> pos
        | _, [] -> lesson_i
      in
      process_chapters
        ~chapters:
          ({
             position = i;
             title = chapter.title;
             reset_lesson_position = chapter.reset_lesson_position;
             lessons = List.rev lessons;
           }
           :: chapters)
        ~i:(i + 1) ~lesson_i rest_chapters

(* the end result *)
let chapters = process_chapters input

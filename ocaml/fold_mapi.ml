(*
    A short immutable solution that uses fold_mapi function. 
    Since the function absent in ocaml stdlib, but present in Base, I kinda re-implemented it here. 
*)

module Lesson = struct 
    type t = {
        name: string;
        position: int option;
    }
end

module Course = struct 
    type t = {
        title: string;
        reset_lesson_position: bool;
        position: int option;
        lessons: Lesson.t list;
    }

    let to_string c = 
        let position p = Option.value ~default: "<undefined>" (Option.map Int.to_string p) in
        let lessons = c.lessons |> List.map (fun lesson -> 
            String.concat "\n" ["- name:" ^ Lesson.(lesson.name); "  position:" ^ (position lesson.position)]
        ) in
        let courses = [
            "title:" ^ c.title;
            "reset_lesson_position:" ^ (Bool.to_string c.reset_lesson_position);
            "position:" ^ (position c.position);
            "lessons:"
        ] in
        String.concat "\n" (courses @ lessons)
end

let input = Course.[
    {
        title = "Getting started";
        reset_lesson_position = false;
        position = None;
        lessons = Lesson.[
            {name = "Welcome"; position = None};
            {name = "Installation"; position = None}
        ]
    };
    {
        title = "Basic Operator";
        reset_lesson_position = false;
        position = None;
        lessons = Lesson.[
            {name = "Addition / Subtraction"; position = None};
            {name = "Multiplication / Division"; position = None}
        ]
    };
    {
        title = "Advanced topics";
        reset_lesson_position = true;
        position = None;
        lessons = Lesson.[
            {name = "Mutability"; position = None};
            {name = "Immutability"; position = None}
        ]
    };
]

let fold_mapi ~init ~f list = 
    let rec loop' idx result acc = function
        | [] -> (acc, List.rev result)
        | a :: rest ->
            let (acc', a') = f idx acc a in
            loop' (idx + 1) (a' :: result) acc' rest
    in loop' 0 [] init list

let transform courses = 
    let (_ , result) = fold_mapi courses ~init: 1 ~f:(fun coursePos n course ->
        let pos = if Course.(course.reset_lesson_position) then 1 else n in
        let lessons = course.lessons |> List.mapi (fun idx lesson -> Lesson.{lesson with position = Some (idx + pos)}) in
        (pos + List.length lessons, {course with position = Some (coursePos + 1); lessons}))
    in result

let () =
    let output = transform input in
    print_endline "INPUT:";
    print_endline (List.map Course.to_string input |> String.concat "\n\n");
    print_endline "\n";
    print_endline "OUTPUT:";
    print_endline (List.map Course.to_string output |> String.concat "\n\n");
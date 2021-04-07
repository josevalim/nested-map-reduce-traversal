type input_lesson = {
  name: string;
}

type input_section = {
  title: string ;
  reset_lesson_position: bool;
  lessons: input_lesson list;
}

type output_lesson = {
  name: string;
  position: int;
}

type output_section = {
  title: string ;
  reset_lesson_position: bool;
  lessons: output_lesson list;
  position: int;
}

let input: input_section list =
  [
    {
      title =  "Getting started";
      reset_lesson_position = false;
      lessons = [
        {
          name = "Welcome";
        };
        {
          name = "Installation";
        };
      ];
    };
    {
      title =  "Basic operator";
      reset_lesson_position = false;
      lessons = [
        {
          name = "Addition / Subtraction";
        };
        {
          name = "Multiplication / Division";
        };
      ];
    };
    {
      title =  "Advanced topics";
      reset_lesson_position = true;
      lessons = [
        {
          name = "Mutability";
        };
        {
          name = "Immutability";
        };
      ];
    };
  ]

let map_with_memo (map_fn: 'm -> 'a -> 'b * 'm) (initial_memo: 'm) (collection: 'a list) : 'b list =
  List.fold_left
    (fun (acc, memo) item ->
       let
         (next_item, next_memo) = map_fn memo item
       in
       (next_item::acc, next_memo)
    )
    ([], initial_memo)
    collection
  |> fst
  |> List.rev

type memo = {
  current_section_index: int;
  current_lesson_index: int;
}

let map_section (memo: memo) (section: input_section) : output_section * memo =
  let
    section_index =
        memo.current_section_index + 1
  in
  let
    current_lesson_index =
        if section.reset_lesson_position then
            0
        else
            memo.current_lesson_index

  in
  let
    next_lessons =
        List.mapi
        (fun ix (lesson: input_lesson) : output_lesson ->
            {
            name = lesson.name;
            position = current_lesson_index + ix + 1;
            }
        )
        section.lessons;
  in
  let
    next_lesson_index =
        memo.current_lesson_index + List.length section.lessons
  in
  (
    {
      title = section.title;
      reset_lesson_position = section.reset_lesson_position;
      position = section_index;
      lessons = next_lessons;
    },
    {
      current_section_index = section_index;
      current_lesson_index = next_lesson_index;
    }
  )

let output =
  map_with_memo
    map_section
    {
      current_section_index = 0 ;
      current_lesson_index = 0;
    }
    input

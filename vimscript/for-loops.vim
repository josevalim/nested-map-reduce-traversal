let input = [
      \   {
      \     "title": "Getting started",
      \     "reset_lesson_position": v:false,
      \     "lessons": [
      \       {"name": "Welcome"},
      \       {"name": "Installation"}
      \     ]
      \   },
      \ 
      \   {
      \     "title": "Basic operator",
      \     "reset_lesson_position": v:false,
      \     "lessons": [
      \       {"name": "Addition / Subtraction"},
      \       {"name": "Multiplication / Division"}
      \     ]
      \   },
      \ 
      \   {
      \     "title": "Advanced topics",
      \     "reset_lesson_position": v:true,
      \     "lessons": [
      \       {"name": "Mutability"},
      \       {"name": "Immutability"}
      \     ]
      \   }
      \ ]

let section_position = 0
let lesson_position = 0

for section in input
  let section_position += 1
  let section.position = section_position

  if section.reset_lesson_position
    let lesson_position = 0
  endif

  for lesson in section.lessons
    let lesson_position += 1
    let lesson.position = lesson_position
  endfor
endfor

echom json_encode(input)

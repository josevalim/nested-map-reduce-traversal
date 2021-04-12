::  Run from dojo with '+nested'
::
:-  %say
|=  *
=<  [%noun traverse]
=>  |%
    +$  lesson  [name=@t position=(unit @ud)]
    +$  entry   [title=@t reset=? position=(unit @ud) lessons=(list lesson)]
    --

=,  dejs:format
::  Main
::
|%
++  traverse
  =/  input=(list entry)
    :~  :*  'Getting started'
            %.n
            ~
            ~[['Welcome' ~] ['Installation' ~]]
        ==
      ::
        :*  'Basic operator'
            %.n
            ~
            ~[['Addition / Subtraction' ~] ['Multiplication / Division' ~]]
        ==
      ::
        :*  'Advanced topics' 
            %.y
            ~
            ~[['Mutability' ~] ['Immutability' ~]]
    ==  ==
  =+  [i=1 lesson-i=1]
  |^
  |-  ^+  input
  ?~  input  ~
  =/  [new-lesson-pos=@ new-entry=entry]
    (modify i.input i lesson-i)
  [new-entry $(i +(i), input t.input, lesson-i new-lesson-pos)]
  ::
  ++  modify
    |=  [=entry position=@ lesson-pos=@]
    ^-  [new-lesson-pos=@ new-entry=^entry]
    =*  reset  reset.entry
    |^
    =+  update-lessons
    :-  ?:(reset 1 pos)
    entry(position (some position), lessons lessons)
    ::
    ++  update-lessons
      %+  roll  lessons.entry
      |=  [=lesson i=_1 pos=_lesson-pos lessons=(list lesson)]
      :+  +(i)
        +(pos)
      (snoc lessons lesson(position ?:(reset (some i) (some pos))))
    --
  --
--

Require Import String.
Require Import List.
Import ListNotations.

(* a lesson has an optional position and a name *)
Inductive lesson := Lesson : option nat -> string -> lesson.

(* a section has an optional position, a title, a reset flag and a list of lessons *)
Inductive section := Section : option nat -> string  -> bool -> list lesson -> section.

(* function to fold over the list of lessons
   the accumulator holds the list of numbered lessons and a lesson counter *)
Definition number_lesson (acc: list lesson * nat) (l:lesson) :=
  match (acc, l) with
  | ((ls, nl), Lesson _ name) => (ls ++ [Lesson (Some (nl + 1)) name], nl + 1)
  end.

(* function to fold over the list of sections
   the accumulator holds the list of numbered sections, a section counter and a lesson counter *)
Definition number_section (acc: list section * nat * nat) (s:section) :=
  match (acc, s) with
  | ((ss, ns, nl), Section _ title reset lessons) =>
    let nl' := match reset with true => 0 | _ => nl end in
    let (lessons', nl'') := List.fold_left number_lesson lessons ([], nl') in
    (ss ++ [Section (Some (ns + 1)) title reset lessons'], ns + 1, nl'')
  end.

(* initial section accumulator *)
Definition acc0 : list section * nat * nat := ([], 0, 0).

(* example input *)
Definition sections : list section := [
    Section None "Getting started" false [
        Lesson None "Welcome";
        Lesson None "Installation"
    ];
    Section None "Basic operator" false [
        Lesson None "Addition / Subtraction";
        Lesson None "Multiplication / Division"
    ];
    Section None "Advanced topics" true [
        Lesson None "Mutability";
        Lesson None "Immutability"
    ]
].

Definition result : list section :=
  match List.fold_left number_section sections acc0 with
  | (ss,_ ,_) => ss
  end.

Compute result.

(*
$ coqc fold.v
     = [Section (Some 1) "Getting started" false
          [Lesson (Some 1) "Welcome"; Lesson (Some 2) "Installation"];
       Section (Some 2) "Basic operator" false
         [Lesson (Some 3) "Addition / Subtraction";
         Lesson (Some 4) "Multiplication / Division"];
       Section (Some 3) "Advanced topics" true
         [Lesson (Some 1) "Mutability"; Lesson (Some 2) "Immutability"]]
     : list section
*)

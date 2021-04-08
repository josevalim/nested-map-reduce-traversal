(* Implementation using a final-tagless representation.
 * This is somewhat common among DSL-solutions in OCaml
 *)

module JSON = struct
  (* This part implements JSON output. A real solution is probably
   * using something like YoJson or likewise here. It serves as the output
   * type as well
   *)
  type t =
    | JString of string
    | JInt of int
    | JList of t list
    | JBool of bool
    | JDict of (string * t) list
end

(* Define the data at the type-level as functions you compose together.
 *
 * Defining a parser from a concrete structure into these functions is an
 * exercise left for the reader.
 *)
module type SYM = sig
  type out = JSON.t
  type 'a repr
  type lesson
  type chapter
  type db
  val lesson : string -> lesson repr
  val chapter : string -> bool -> (lesson repr) list -> chapter repr
  val db: (chapter repr) list -> db repr

  val eval : db repr -> out
end

(* The example, given as abstract data. Note that we define this as a functor,
 * so we don't say how the functions db, chapter, lesson, ... are evaluated
 *)
module Ex1(S: SYM) = struct
  open S
  let ex1 = db [chapter "Getting started" false [lesson "Welcome"; lesson "Installation"];
                chapter "Basic operator" false [lesson "Addition / Subtraction"; lesson "Multiplication / Division"];
                chapter "Advanced topics" true [lesson "Mutability"; lesson "Immutability"]]
  let ex1_eval = eval ex1
end

(* Provide an evaluation interpreter over the structure *)
module Eval : SYM = struct
  type out = JSON.t
  (* The representation here is key. It states we thread the positional arguments around.
   * A more CPS-like variant are also possible here by manipulating this type *)
  type 'a repr = int -> int -> (int * int * 'a)
  type lesson = JSON.t
  type chapter = JSON.t
  type db = JSON.t

  let lesson l i j = (i, j+1, JSON.JDict [("name", JString l); ("position", JInt j)])

  (* Unexported helper for a list of lessons *)
  let lessons (ls: lesson repr list) i j =
    let (final_i, final_j, xs) =
      (List.fold_left
        (fun (i, j, acc) less -> let (i', j', out) =  less i j in (i', j', out::acc))
        (i,j, [])
        ls)
    in (final_i, final_j, JSON.JList (List.rev xs))

  (* The following two functions make aggressive use of binding shadowing. It's intended *)
  let chapter title reset ls i j =
    let j = if reset then 1 else j in
    let (i, j, less_out) = lessons ls i j
    in (i+1, j, JSON.JDict [("title", JString title); ("position", JInt i); ("reset_lesson_position", JBool reset); ("lessons", less_out)])
       
  let db (chapters: (chapter repr) list) i j =
    let rec loop cs i j acc =
      match cs with
      | [] -> (i, j, List.rev acc)
      | c::next ->
        let (i, j, d) = c i j in loop next i j (d::acc) in
    let (i, j, xs) = loop chapters i j []
    in (i, j, JSON.JList xs)

  let eval db =
    let (_, _, res) = db 1 1 in res
end


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
  type section
  type db
  val lesson : string -> lesson repr
  val section : string -> bool -> (lesson repr) list -> section repr
  val db: (section repr) list -> db repr

  val eval : db repr -> out
end

(* The example, given as abstract data. Note that we define this as a functor,
 * so we don't say how the functions db, section, lesson, ... are evaluated
 *)
module Ex1(S: SYM) = struct
  open S
  let ex1 = db [section "Getting started" false
                        [lesson "Welcome";
                         lesson "Installation"];
                section "Basic operator" false
                        [lesson "Addition / Subtraction";
                         lesson "Multiplication / Division"];
                section "Advanced topics" true
                        [lesson "Mutability";
                         lesson "Immutability"]]
  let ex1_eval = eval ex1
end

(* Provide an evaluation interpreter over the structure. This particular version uses a
  monad  *)
module Eval : SYM = struct
  (* The track module implements a monad over the state of the positions *)
  module Track = struct
    (* Monad base type. Given a state of (i,j) compute its action *)
    type 'a t = (int * int) -> ('a * (int * int))

    (* Standard monad operations *)
    let return x (i,j) = x, (i, j)
    let bind act k s =
      let (iv, is) = act s
      in k iv is

    (* The special operations for this monad. we can increment i and j,
     * and we can reset j *)
    let inc_i (i,j) = i, (i+1,j)
    let inc_j (i,j) = j, (i,j+1)
    let reset_j (i, _j) = (), (i,1)

    (* Use let* syntax in the following *)
    let ( let* ) x f = bind x f

    (* mapM is the monadic version of a map. It threads the monadic operation
     * through. *)
    let mapM f xs =
      let k x r =
          let* fx = f x in
          let* xs = r in
          return (fx::xs)
      in List.fold_right k xs (return [])

    (* In practice, we just need mapM for it's effect, hence this helper *)
    let sequence = mapM (fun x -> x)

    (* Run a monad, small helper *)
    let run i j f =
      let (r, _) = f (i,j) in
      r
  end

  type out = JSON.t
  type 'a repr = 'a Track.t (* The representation is a monad, i.e., a program *)
  type lesson = JSON.t
  type section = JSON.t
  type db = JSON.t

  (* With the above scaffold in place, the actual code is simple: *)
  let lesson l =
    Track.(
      let* pos = inc_j in
      return (JSON.JDict [("name", JString l); ("position", JInt pos)])
    )

  let section title reset ls =
    Track.(
      let* () = if reset then reset_j else return () in
      let* i = inc_i in
      let* less_out = sequence ls in
      return (JSON.JDict [("title", JString title);
                          ("position", JInt i);
                          ("reset_lesson_position", JBool reset);
                          ("lessons", JSON.JList less_out)])
    )

  let db ss =
    Track.(
      let* data = sequence ss in
      return (JSON.JList data)
    )

  let eval db = Track.run 1 1 db
end

(* Another implementation. This one is the straightforward one, using ref cells in OCaml *)
module EvalRef : SYM = struct
  (* Reference cells *)
  let pos_i = ref 1
  let pos_j = ref 1

  (* Same helpers as in the monadic case *)
  let inc_i () =
    let i = !pos_i in
    pos_i := i + 1;
    i

  let inc_j () =
    let j = !pos_j in
    pos_j := j + 1;
    j

  let reset_j () =
    pos_j := 1

  type out = JSON.t
  type 'a repr = unit -> JSON.t (* The representation is function *)
  type lesson = JSON.t
  type section = JSON.t
  type db = JSON.t

  let lesson l () =
    let pos = inc_j () in
    (JSON.JDict [("name", JString l); ("position", JInt pos)])

  let section title reset ls () =
      if reset then reset_j ();
      let i = inc_i () in
      let less_out = List.map (fun f -> f ()) ls in

      JSON.JDict [("title", JString title); ("position", JInt i);
                  ("reset_lesson_position", JBool reset);
                  ("lessons", JSON.JList less_out)]

  let db ss () =
    let data = List.map (fun f -> f ()) ss in
    JSON.JList data

  let eval db =
    pos_i := 1;
    pos_j := 1;
    db ()
end

(* Example of use. Instantiate the two variants *)
module R = Ex1(Eval)
module R2 = Ex1(EvalRef)

(* Test that there's agreement *)
let _ =
  if R.ex1_eval = R2.ex1_eval
  then print_endline "Ok."
  else print_endline "Mismatch."

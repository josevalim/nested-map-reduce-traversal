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

(* Provide an evaluation interpreter over the structure. This particular version uses a
  monad  *)
module Eval : SYM = struct
  (* The track module implements a monad over the state of the positions *)
  module Track = struct
    (* Monad base type. Given a state of (i,j) compute its action *)
    type 'a t = RunState of ((int * int) -> ('a * (int * int)))

    (* Standard monad operations *)
    let return x = RunState (fun (i,j) -> (x, (i, j)))
    let bind (act: 'a t) (k: 'a -> 'b t) : 'b t = RunState (fun s ->
      let RunState fact = act in
      let (iv, is) = fact s in
      let RunState fk = k iv in
      fk is)

    (* The special operations for this monad. we can increment i and j,
     * and we can reset j *)
    let inc_i = RunState (fun (i,j) -> (i, (i+1,j)))
    let inc_j = RunState (fun (i,j) -> (j, (i,j+1)))
    let reset_j = RunState (fun (i,_j) -> ((), (i,1)))

    (* Use let* syntax in the following *)
    let ( let* ) x f = bind x f

    (* mapM is the monadic version of a map. It threads the monadic operation
     * through. *)
    let mapM f xs =
      let k x r =
          let* x = f x in
          let* xs = r in
          return (x::xs)
      in List.fold_right k xs (return [])

    (* In practice, we just need mapM for it's effect, hence this helper *)
    let sequence = mapM (fun x -> x)

    (* Run a monad, small helper *)
    let run i j x =
      let RunState f = x in
      let (r, _) = f (i,j) in
      r

  end

  type out = JSON.t
  type 'a repr = 'a Track.t (* The representation is a monad, i.e., a program *)
  type lesson = JSON.t
  type chapter = JSON.t
  type db = JSON.t

  (* With the above scaffold in place, the actual code is simple: *)
  let lesson l : lesson repr =
    Track.(
      let* pos = inc_j in
      return (JSON.JDict [("name", JString l); ("position", JInt pos)])
    )

  let chapter title reset ls =
    Track.(
      let* () = if reset then reset_j else return () in
      let* i = inc_i in
      let* less_out = sequence ls in
      return (JSON.JDict [("title", JString title); ("position", JInt i); ("reset_lesson_position", JBool reset); ("lessons", JSON.JList less_out)])
    )

  let db cs =
    Track.(
      let* data = sequence cs in
      return (JSON.JList data)
    )

  let eval db = Track.run 1 1 db
end

module R = Ex1(Eval)
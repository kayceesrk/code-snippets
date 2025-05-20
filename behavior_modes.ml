(*
 * Modes version of https://kcsrk.info/ocaml/types/2016/06/30/behavioural-types/
 * Runs on OCaml compiler with modes support: https://github.com/ocaml-flambda/flambda-backend
 *)

module type Ref =
sig
  type ('a, 'b) ref constraint 'b = [>]
  (* 'b is the behavioural type variable *)

  val ref   : 'a -> ('a, 'b) ref @ unique
  val read  : ('a, [`Read of 'b]) ref @ unique
              -> 'a Modes.Aliased.t * ('a, 'b) ref @ unique
  val write : ('a, [`Write of 'b]) ref @ unique
              -> 'a
              -> ('a, 'b) ref @ unique
  val branch : ('a, [>] as 'b) ref @ unique
               -> (('a, [>] as 'c) ref @ unique -> 'b)
               -> ('a, 'c) ref @ unique
end

module Ref : Ref =
struct

  type ('a, 'b) ref = {contents : 'a @@ aliased} constraint 'b = [>]

  let ref : 'a -> ('a, 'b) ref @ unique = fun v -> unique_ {contents = v}

  let read (r @ unique) : _ @ unique =
    let c = Modes.Aliased.{aliased = r.contents} in
    c, {contents = r.contents}

  let write r v =
    { contents = v }

  let branch r _ = Obj.magic_at_unique r
end

let foo1 r =
  let r = Ref.write r 20 in
  Ref.read r

(* The inferred type of [foo1] is

   val foo1 :
   (int, [ `Write of [ `Read of [>  ] as 'a ] ]) Ref.ref @ unique ->
   int Modes.Aliased.t * (int, 'a) Ref.ref

   The type says that [foo1] first writes and then reads the reference. 
   This may be followed by any action. *)

let v, res_ref =
  (* This reference can only be written once followed by a single read *)
  let my_ref1 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref @ unique = Ref.ref 10 in
  foo1 my_ref1

(*
  val v : int Modes.Aliased.t = {Modes.Aliased.aliased = 20}
  val res_ref : (int, [ `Stop ]) Ref.ref = <abstr>
*)

let rec foo2 r =
  let r = Ref.write r 20 in
  let v, r = Ref.read r in
  foo2 r

(* Recursive types are obtained painlessly. The type of [foo2] is:

   val foo2 : (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref @ unique -> 'b

   Of course, this loops forever. *)

let _ =
  let my_ref2 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref @ unique = Ref.ref 10 in
(*   foo2 my_ref2; (* UNCOMMENT TO SEE ERROR *) *)
  ignore my_ref2

(* The snippet above will not type check when the second line is uncommented.
   The error is:

File "behavior_modes.ml", line 71, characters 7-14:
71 |   foo2 my_ref2
            ^^^^^^^
Error: This expression has type
         (int, [ `Write of [ `Read of [ `Stop ] ] ]) Ref.ref
       but an expression was expected of type
         (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref
       These two variant types have no intersection
*)

let test_myref3 () =
  let my_ref3 : (int, [`Write of [`Read of [>]]]) Ref.ref = Ref.ref 10 in
  (* [my_ref3] must be written and read once and then any subsequent operation
     can be performed! *)
  let _ = foo2 my_ref3 in (* foo2 accepts my_ref3, and runs forever! *)
  ()

let rec foo3 r = function
  | 0 ->
      print_endline "done";
      Ref.write (Ref.branch r (fun x -> `Zero x)) 0
  | n ->
      let r = Ref.write (Ref.branch r (fun x -> `Succ x)) 20 in
      let v, r = Ref.read r in
      foo3 r (n-1)

(* The type of [foo3] is:

val foo3 :
  (int,
   [> `Succ of (int, [ `Write of [ `Read of 'a ] ]) Ref.ref
    | `Zero of (int, [ `Write of [>  ] as 'b ]) Ref.ref ]
   as 'a)
  Ref.ref @ unique -> int -> (int, 'b) Ref.ref

   The inferred type captures the branching behaviour. The argument to the
   branch is always of the form  [fun x -> `Tag x].

*)

let test_myref4 () =
  let my_ref4 = Ref.ref 10 in
  foo3 my_ref4 32
(* Works as expected *)

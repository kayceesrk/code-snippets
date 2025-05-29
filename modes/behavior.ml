(* Fun with behavioral types. Inspired by https://hal.archives-ouvertes.fr/hal-01216310 *)

exception LinearityViolation
let (|>) v f = f v

module type Ref =
sig
  type ('a, 'b) ref constraint 'b = [>]
  (* 'b is the behavioural type variable *)

  val ref   : 'a -> ('a, 'b) ref
  val read  : ('a, [`Read of 'b]) ref -> 'a * ('a, 'b) ref
  (* Require `Read capability. 'b is the continuation's behavior. *)
  val write : ('a, [`Write of 'b]) ref -> 'a -> ('a, 'b) ref
  val branch : ('a, [>] as 'b) ref -> (('a, [>] as 'c) ref -> 'b) -> ('a, 'c) ref
end

module Ref : Ref =
struct

  type ('a, 'b) ref =
    {contents     : 'a;
     mutable live : bool} (* For linearity *)
     constraint 'b = [>]

  let ref v = {contents = v; live = true}

  let check r =
    if not r.live then raise LinearityViolation;
    r.live <- false

  let fresh r = {r with live = true}

  let read r =
    check r;
    (r.contents, fresh r)

  let write r v =
    check r;
    { contents = v; live = true }

  let branch r _ = check r; fresh r
end

(* This reference can only be written once followed by a single read *)
let my_ref1 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref = Ref.ref 10

let foo1 r =
  let r = Ref.write r 20 in
  Ref.read r

(* The inferred type of foo1 is
 *
 * val foo1 : (int, [ `Write of [ `Read of [>  ] as 'a ] ]) Ref.ref -> int * (int, 'a) Ref.ref
 *
 * which says that foo1 writes and reads the ref. *)

let v,res_ref = foo1 my_ref1

(* val v : int = 20
 * val res_ref : (int, [ `Stop ]) Ref.ref
 *)

let rec foo2 r =
  let r = Ref.write r 20 in
  let v, r = Ref.read r in
  foo2 r

(*
 * val foo2 : (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref -> 'b
 *
 * Recursive types are obtained painlessly. Ofcourse, this loops forever. *)

let my_ref2 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref = Ref.ref 10
(* let _ = foo2 my_ref2 *)

(* Error: This expression has type
 *          (int, [ `Write of [ `Read of [ `Stop ] ] ]) Ref.ref
 *        but an expression was expected of type
 *          (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref
 *        These two variant types have no intersection
 *)

let test () =
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

(* val foo3 :
 *  (int,
 *   [> `Succ of (int, [ `Write of [ `Read of 'a ] ]) Ref.ref
 *    | `Zero of (int, [ `Write of [>  ] as 'b ]) Ref.ref ]
 *  as 'a)
 *
 * The inferred type captures branching behavior. The argument to the branch is
 * always of the form << fun x -> `Tag x >>.
 *)

let test () =
  let my_ref4 = Ref.ref 10 in
  foo3 my_ref4 32
(* Works as expected *)

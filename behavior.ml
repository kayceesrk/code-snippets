(* Fun with behavioral types. Inspired by https://hal.archives-ouvertes.fr/hal-01216310 *)

module type Ref =
sig
  type ('a, 'b) ref constraint 'b = [>]
  (* 'b is the behavioural type variable *)

  val ref   : 'a -> ('a, 'b) ref
  val read  : ('a, [`Read of 'b]) ref -> 'a * ('a, 'b) ref
  (* Require `Read capability. 'b is the continuation's behavior. *)
  val write : ('a, [`Write of 'b]) ref -> 'a -> ('a, 'b) ref

  val left : ('a, [`Left of 'b | `Right of 'c]) ref -> ('a, 'b) ref
  val right : ('a, [`Left of 'b | `Right of 'c]) ref -> ('a, 'c) ref
end

module Ref : Ref =
struct

  type ('a, 'b) ref =
    {contents     : 'a;
     mutable live : bool} (* For linearity *)
     constraint 'b = [>]

  let ref v = {contents = v; live = true}

  let read r =
    assert (r.live);
    r.live <- false;
    (r.contents, { r with live = true })

  let write r v =
    assert (r.live);
    r.live <- false;
    { contents = v; live = true }

  let left {contents; live} = {contents; live}
  let right {contents; live} = {contents; live}
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

let my_ref3 : (int, [`Write of [`Read of [>]]]) Ref.ref = Ref.ref 10
(* my_ref3 must be written and read once and then any subsequent operation can be performed! *)
(* let _ = foo2 my_ref3 (* foo2 accepts my_ref3, and runs forever! *) *)

let rec foo3 r = function
  | 0 -> Ref.write (Ref.left r) 0
  | n ->
      let r = Ref.write (Ref.right r) 20 in
      let v, r = Ref.read r in
      foo3 r (n-1)

(* val foo3 :
 *   (int,
 *    [ `Test of [ `Write of [>  ] as 'b ] * [ `Write of [ `Read of 'a ] ] ] as 'a)
 *   Ref.ref -> int -> (int, 'b) Ref.ref
 *
 * The inferred type accounts for the fact that the branches do different
 * actions. However, this interface isn't great; I don't like the fact that the
 * reference needs to be threaded through. It might be troublesome in the presence
 * of multiple references.
 *)


let my_ref4 = Ref.ref 10
let _ = foo3 my_ref4 32
(* Works as expected *)

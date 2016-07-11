module type Tree =
sig
  type ('a,+'b,+'c) tree
    constraint 'b = [>] (* shape *)
    constraint 'c = [>] * [>] (* count as difference between peano numbers *)

  val mk_leaf  : unit -> ('a,[`Leaf], 'z * 'z) tree
  val mk_node  : ('a, 'b, 'u*'n) tree -> 'a -> ('a, 'c, 'm*'u) tree ->
    ('a, [`Node of ('b *'u * 'n) * ('c * 'm * 'u)], 'm*[`Succ of 'n]) tree

  val value : ('a, [`Node of _] as 'b, 'c) tree -> 'a * ('a, 'b, 'c) tree
  val left  : ('a, [`Node of ('b * 'u * 'n) * _] as 'd, 'f) tree ->
    ('a, 'b, 'u*'n) tree * ('a, 'd, 'f) tree
  val right : ('a, [`Node of _ * ('b * 'u * 'n)] as 'd, 'f) tree ->
    ('a, 'b, 'u * 'n) tree * ('a, 'd, 'f) tree
  val destruct : ('a, [< `Leaf | `Node of 'b * 'c], 'f) tree ->
    [`L | `N of ('a,[`Node of 'b * 'c], 'f) tree]
  val strip : ('a, _,_) tree -> ('a, _, _) tree
end

module Tree : Tree =
struct
  type 'a _tree = Leaf | Node of 'a _tree * 'a * 'a _tree
  type ('a,+'b,+'c) tree = ('a _tree * bool ref)
    constraint 'b = [>]
    constraint 'c = [>] * [>]

  let fresh x = (x, ref true)
  exception LinearityViolation
  let check r = if not !r then raise LinearityViolation else r := false

  let mk_leaf () = fresh Leaf
  let mk_node (l,s1) v (r,s2) =
    check s1; check s2;
    fresh (Node (l, v, r))

  let value (t,r) =
    check r;
    match t with
    | Leaf -> failwith "impossible"
    | (Node (_,v,_)) as n -> (v, fresh n)

  let left (t,r) =
    check r;
    match t with
    | Leaf -> failwith "impossible"
    | (Node (l,_,_)) as n -> (fresh l, fresh n)

  let right (t,r) =
    check r;
    match t with
    | Leaf -> failwith "impossible"
    | (Node (_,_,r)) as n -> (fresh r, fresh n)

  let destruct (t,r) =
    check r;
    match (Obj.magic t) with
    | Leaf -> `L
    | Node (l,v,r) -> `N (fresh (Node (l,v,r)))

  let strip(t,r) = check r; fresh t
end

open Tree

let rec preorder t f =
  match destruct (strip t) with
  | `L -> ()
  | `N t ->
      let v, t = value t in
      f v;
      let l , t = left t in
      preorder l f;
      let r, t = right t in
      preorder r f

(* Right branching tree *)
let make_tree1 () =
  mk_node
    (mk_leaf ())
    10
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ())
    )


(* balanced tree *)
let make_tree2 () =
  mk_node
    (mk_node
      (mk_leaf ())
      10
      (mk_leaf ()))
    10
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ()))

let make_tree3 () =
  mk_node
    (mk_node
      (mk_node
        (mk_leaf ())
        40
        (mk_leaf ()))
      10
      (mk_leaf ()))
    10
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ()))

let _ = preorder (make_tree1 ()) (fun x -> Printf.printf "%d\n" x)
let _ = preorder (make_tree2 ()) (fun x -> Printf.printf "%d\n" x)
let _ = preorder (make_tree3 ()) (fun x -> Printf.printf "%d\n" x)

module Balanced = struct
  type ('a,'b,'c) balanced_tree = ('a,'b,'c) tree
    constraint 'b = [< `Leaf | `Node of ('d * 'u * 'n) * ('d * 'm * 'u)]
end

module RightBranching = struct
  type ('a,+'b,+'c) right_branching = ('a,'b,'c) tree
    constraint 'b = [`Leaf | `Node of [`Leaf] * 'b]
end

module LeftBranching = struct
  type ('a,+'b,+'c) left_branching = ('a,'b,'c) tree
     constraint 'b = [`Leaf | `Node of 'b * [`Leaf]]
end


let t2 = make_tree2()
let x : (_,_,_) Balanced.balanced_tree = t2
(* The following do not type check

let x : (_,_) Balanced.balanced_tree = make_tree1 ()
let x : (_,_) Balanced.balanced_tree = make_tree3 ()
*)

(* Needs explicit casting *)
let y : (_,_,_) RightBranching.right_branching =
  (make_tree1 () : (('a,[`Node of ([`Leaf] * _ * _) * [`Node of [`Leaf] * [`Leaf]]], 'c) tree) :>
                   (('a,[`Leaf | `Node of [`Leaf] * 'b] as 'b, 'c) tree))

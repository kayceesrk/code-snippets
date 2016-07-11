module type Tree =
sig
  type ('a,+'b) tree constraint 'b = [>]

  val mk_leaf  : unit -> ('a,[`Leaf]) tree
  val mk_node  : ('a, 'b) tree -> 'a -> ('a, 'c) tree -> ('a, [`Node of 'b * 'c ]) tree

  val value : ('a, [`Node of _] as 'b) tree -> 'a * ('a, 'b) tree
  val left  : ('a, [`Node of 'b * _] as 'd) tree ->
    ('a, 'b) tree * ('a, 'd) tree
  val right : ('a, [`Node of _ * 'b] as 'd) tree ->
    ('a, 'b) tree * ('a, 'd) tree
  val destruct : ('a, [< `Leaf | `Node of 'b * 'c]) tree ->
    [`L | `N of ('a,[`Node of 'b * 'c]) tree]
  val strip_shape : ('a, _) tree -> ('a, _) tree
end

module Tree : Tree =
struct
  type 'a _tree = Leaf | Node of 'a _tree * 'a * 'a _tree
  type ('a,+'b) tree = ('a _tree * bool ref) constraint 'b = [>]

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

  let strip_shape(t,r) = check r; fresh t
end

open Tree

let rec preorder t f =
  match destruct (strip_shape t) with
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
  type ('a,+'b) balanced_tree = ('a,'b) tree
    constraint 'b = [< `Leaf | `Node of 'c * 'c]
end

module RightBranching = struct
  type ('a,+'b) right_branching = ('a,'b) tree
    constraint 'b = [`Leaf | `Node of [`Leaf] * 'b]
end

module LeftBranching = struct
  type ('a,+'b) left_branching = ('a,'b) tree
     constraint 'b = [`Leaf | `Node of 'b * [`Leaf]]
end

let x : (_,_) Balanced.balanced_tree = make_tree2 ()
(* The following do not type check

let x : (_,_) Balanced.balanced_tree = make_tree1 ()
let x : (_,_) Balanced.balanced_tree = make_tree3 ()
*)

(* Needs explicit casting *)
let y : (_,_) RightBranching.right_branching =
  (make_tree1 () : (('a,[`Node of [`Leaf] * [`Node of [`Leaf] * [`Leaf]]]) tree) :>
                   (('a,[`Leaf | `Node of [`Leaf] * 'b] as 'b) tree))

module type Tree =
sig
  type ('a,+'b,+'c) tree
    constraint 'b = [>] (* shape *)
    constraint 'c = [>] (* control *)

  val mk_leaf  : unit -> ('a,[`Leaf], 'c) tree
  val mk_node  : ('a, 'b, 'x) tree -> 'a -> ('a, 'c, 'y) tree ->
    ('a, [`Node of ('b * 'x) * ('c * 'y)], _) tree

  val value : ('a, [`Node of _] as 'b, [`Value of 'c]) tree -> 'a * ('a,'b,'c) tree
  val left  : ('a, [`Node of ('b * 'x) * _] as 'd, [`Left of 'c]) tree ->
    ('a, 'b, 'x) tree * ('a, 'd, 'c) tree
  val right : ('a, [`Node of _ * ('b * 'x)] as 'd, [`Right of 'c]) tree ->
    ('a, 'b, 'x) tree * ('a, 'd, 'c) tree
  val destruct : ('a, [< `Leaf | `Node of 'b * 'c], 'd) tree ->
    [`L | `N of ('a,[`Node of 'b * 'c], 'd) tree]

  val strip_shape : ('a, _, 'b) tree -> ('a, _, 'b) tree
end

module Tree : Tree =
struct
  type 'a _tree = Leaf | Node of 'a _tree * 'a * 'a _tree
  type ('a,+'b, +'c) tree = ('a _tree * bool ref)
    constraint 'b = [>]
    constraint 'c = [>]

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

let rec inorder t f =
  match destruct (strip_shape t) with
  | `L -> ()
  | `N t ->
      let l, t = left t in
      inorder l f;
      let v, t = value t in
      f v;
      let r, t = right t in
      inorder r f

let rec postorder t f =
  match destruct (strip_shape t) with
  | `L -> ()
  | `N t ->
      let l, t = left t in
      postorder l f;
      let r, t = right t in
      postorder r f;
      let v, t = value t in
      f v

let make_tree1 () =
  mk_node
    (mk_leaf ())
    10
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ())
    )

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

module Balanced = struct
  type ('a,+'b,+'c) balanced_tree = ('a,'b,'c) tree
    constraint 'b = [< `Leaf | `Node of ('d * _) * ('d * _)]
end

module RightBranching = struct
  type ('a,+'b,+'c) right_branching = ('a,'b,'c) tree
    constraint 'b = [`Leaf | `Node of ([`Leaf] * _) * ('b * _)]
end

module LeftBranching = struct
  type ('a,+'b,+'c) left_branching = ('a,'b,'c) tree
     constraint 'b = [`Leaf | `Node of ('b * _) * ([`Leaf] * _)]
end

let _ = preorder (make_tree2 ()) (fun x -> Printf.printf "%d\n" x)
let x : (_,_,_) Balanced.balanced_tree = make_tree2 ()
(* let x : (_,_,_) Balanced.balanced_tree = make_tree1 () *)
(* let x : (_,_,_) Balanced.balanced_tree = make_tree3 () *)

let y : (_,_,_) RightBranching.right_branching = make_tree1 ()

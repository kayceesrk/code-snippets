module type Tree =
sig
  type ('a,'b) tree

  val mk_leaf  : unit -> ('a,[`Leaf]) tree
  val mk_node  : ('a, 'b) tree -> 'a -> ('a, 'c) tree -> ('a, [`Node of 'b * 'c ]) tree

  val value : ('a, [`Node of _ * _] as 'b) tree -> 'a * ('a, 'b) tree
  val left  : ('a, [`Node of 'b * _] as 'd) tree ->
    ('a, 'b) tree * ('a, 'd) tree
  val right : ('a, [`Node of _ * 'b] as 'd) tree ->
    ('a, 'b) tree * ('a, 'd) tree
  val destruct : ('a, [< `Leaf | `Node of 'b * 'c]) tree ->
    [`L | `N of ('a, [`Node of 'b * 'c]) tree]
end

module Tree : Tree =
struct
  type ('a,'b) tree =
    | Leaf : ('a, [> `Leaf]) tree (* > to be able to write descruct *)
    | Node : ('a,'c) tree * 'a * ('a,'d) tree -> ('a, [> `Node of 'c * 'd]) tree
  let mk_leaf () = Leaf
  let mk_node l v r = Node (l,v,r)

  let left (Node (l,_,_) as t) = l,t
  let value (Node (_,v,_) as t) = v,t
  let right (Node (_,_,r) as t) = r,t

  let destruct t =
    match (Obj.magic t) with
    | Leaf -> `L
    | Node (l,v,r) -> `N (Node (l, v, r))
end

open Tree

let rec preorder t f =
  match destruct t with
  | `L -> ()
  | `N t ->
      let v, t = value t in
      f v;
      let l, t = left t in
      preorder (Obj.magic l) f;
      let r, t = right t in
      preorder (Obj.magic r) f

let make_tree1 () =
  mk_node
    (mk_node
      (mk_leaf ())
      20
      (mk_node
        (mk_leaf ())
        30
        (mk_leaf())
      )
    )
    10
    (mk_leaf ())

let make_tree2 () =
  mk_node
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ()))
    10
    (mk_node
      (mk_leaf ())
      20
      (mk_leaf ()))

module Balanced = struct
  type ('a,'b) balanced_tree = ('a,'b) tree
    constraint 'b = [< `Leaf | `Node of 'c * 'c]
end

module RightBranching = struct
  type ('a,'b) right_branching = ('a,'b) tree
    constraint 'b = [< `Leaf | `Node of [`Leaf] * 'b]
end

let _ = preorder (make_tree2 ()) (fun x -> Printf.printf "%d\n" x)
let x : (_,_) Balanced.balanced_tree = make_tree2 ()

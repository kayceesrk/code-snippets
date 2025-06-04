module type Linear_ref = sig
  type 'a t
  val alloc : 'a -> 'a t @ once
  val free : 'a t @ once -> unit
  val get : 'a t @ once -> 'a * 'a t @ once
  val set : 'a t @ once -> 'a -> 'a t @ once
end

module Linear_ref : Linear_ref = struct
  type 'a t = { mutable value : 'a }
  let alloc x = { value = x }
  let free t = ()
  let get t =
    t.value, t
  let set t x =
    t.value <- x;
    t
end

open Linear_ref

let works () =
  let r = alloc 42 in
  let v,r = get r in
  let r = set r (v + 1) in
  let v,r = get r in
  print_int v;
  free r;
  ()

(*
let fails () =
  let r = alloc 42 in
  free r;
  get r (* fails here *)
*)

let works () =
  let set_to_20 (r @ once) =
    r := 20
  in
  let r @ many = ref 10 in
  set_to_20 r

let wrap () =
  let r = alloc 42 in
  let f () = free r in
  f

module type Unique_ref = sig
  type 'a t
  val alloc : 'a -> 'a t @ unique
  val free : 'a t @ unique -> unit
  val get : 'a t @ unique -> 'a Modes.Aliased.t * 'a t @ unique
  val set : 'a t @ unique -> 'a -> 'a t @ unique
end

module Unique_ref : Unique_ref = struct
  type 'a t = { mutable value : 'a }
  let alloc x = { value = x }
  let free t = ()
  let get t =
    let a = Modes.Aliased.{aliased = t.value } in
    a, t
  let set t x =
    t.value <- x;
    t
end

open Unique_ref
open Modes.Aliased

let works () =
  let t = alloc 42 in (* Allocate a unique reference *)
  free t (* free it *)

(*
let wat () =
  let t = alloc 42 in (* Allocate a unique reference *)
  let f () = free t in (* capture free in a closure *)
  f (); (* free it *)
  f () (* free it again??? *)
*)

let dup r = r,r

let works () =
  let t = alloc 42 in (* Allocate a unique reference *)
  dup t (* duplicate the reference and make it useless *)

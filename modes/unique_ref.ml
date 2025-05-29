module type S = sig
  type 'a t
  val alloc : 'a -> 'a t @ unique
  val free : 'a t @ unique -> unit
  val get : 'a t @ unique -> 'a Modes.Aliased.t * 'a t @ unique
  val set : 'a t @ unique -> 'a -> 'a t @ unique
end

module M : S = struct
  type 'a t = { mutable value : 'a }
  let alloc x = unique_ { value = x }
  let free t = ()
  let get t =
    let a = Modes.Aliased.{aliased = t.value } in
    a, t
  let set t x =
    t.value <- x;
    t
end

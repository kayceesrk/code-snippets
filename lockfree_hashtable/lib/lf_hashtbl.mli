module type EqType = sig
  type t
  val eq : t -> t -> bool
end

module type S = sig
  type key
  type 'a t
  val make : int -> 'a t
  val lookup : 'a t -> key -> 'a option
  val insert : 'a t -> key -> 'a -> [`Already_set of 'a | `Full | `Success ]
end

module Make (Eq : EqType) : S with type key = Eq.t

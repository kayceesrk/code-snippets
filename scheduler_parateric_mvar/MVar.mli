type 'a t
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> unit
val take : 'a t -> 'a

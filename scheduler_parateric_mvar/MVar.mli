type 'a t
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : unit Sched.suspender -> 'a -> 'a t -> unit
val take : 'a Sched.suspender -> 'a t -> 'a

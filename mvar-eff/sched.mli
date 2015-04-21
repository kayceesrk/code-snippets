(** Represents a blocked computation that waits for a value of type 'a. *)
type 'a cont

(** [suspend f] applies [f] to the current continuation, and suspends the
    execution of the current thread, and switches to the next thread in the
    scheduler's queue. *)
val suspend : ('a cont -> unit) -> 'a

(** [resume (k,v)] prepares the suspended continuation [k] with value [v] and
    enqueues it to the scheduler queue. *)
val resume  : 'a cont * 'a -> unit

(** [run f] runs [f] with the cooperative-threaded scheduler. *)
val run : (unit -> unit) -> unit

(** [fork f] forks [f] as a new thread to which control immediately switches to. *)
val fork : (unit -> unit) -> unit

(** [yield ()] suspends the current thread and switches to the next thread from
    the run queue. *)
val yield : unit -> unit

(** [get_tid ()] returns the current thread identifier. *)
val get_tid : unit -> int

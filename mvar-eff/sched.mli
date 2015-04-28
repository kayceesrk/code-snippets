(** Represents a blocked computation that waits for a value of type 'a. *)
type 'a cont

type _ Effects.effect += Suspend : ('a cont -> unit) -> 'a Effects.effect
(** [Perform @@ Suspend f] applies [f] to the current continuation, and suspends the
    execution of the current thread, and switches to the next thread in the
    scheduler's queue. *)

type _ Effects.effect += Resume : 'a cont * 'a -> unit Effects.effect
(** [perform @@ Resume (k,v)] prepares the suspended continuation [k] with value [v] and
    enqueues it to the scheduler queue. *)

type _ Effects.effect += Fork : (unit -> unit) -> unit Effects.effect
(** [perform @@ Fork f] forks [f] as a new thread to which control immediately switches to. *)

type _ Effects.effect += Yield : unit Effects.effect
(** [perform Yield] suspends the current thread and switches to the next thread from
    the run queue. *)

type _ Effects.effect += Get_Tid : int Effects.effect
(** [perform Get_Tid] returns the current thread identifier. *)

val run : (unit -> unit) -> unit
(** [run f] runs [f] with the cooperative-threaded scheduler. *)

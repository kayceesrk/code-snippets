module type S = sig
  (** MVar type. Represents a data structure with a single hole that can be
      filled with value. *)
  type 'a t

  (** [new_mvar v] allocates a new mvar with the hole filled with value [v]. *)
  val new_mvar : 'a -> 'a t

  (** [new_empty_mvar ()] allocates a new mvar with the hole empty. *)
  val new_empty_mvar : unit -> 'a t

  (** [put_mvar v m] fills mvar [m] with value v. If the mvar is already filled,
      this operation blocks until the hole become empty. *)
  val put_mvar : 'a -> 'a t -> unit

  (** [take_mvar m] empties the mvar [m] if it is filled and returns the value.
      If [m] is empty, then the operation blocks until the mvar becomes filled. *)
  val take_mvar : 'a t -> 'a
end

module type SCHED = sig
  type 'a cont
  (** Represents a blocked computation that waits for a value of type 'a. *)

  type _ eff += Suspend : ('a cont -> unit) -> 'a eff
  (** [perform @@ Suspend f] applies [f] to the current continuation, and suspends the
      execution of the current thread, and switches to the next thread in the
      scheduler's queue. *)

  type _ eff += Resume  : 'a cont * 'a -> unit eff
  (** [Perform @@ Resume (k,v)] prepares the suspended continuation [k] with value [v] and
      enqueues it to the scheduler queue. *)
end

module Make (S : SCHED) : S

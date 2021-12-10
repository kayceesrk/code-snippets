module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val suspend : 'a Sched.suspender
  val run : (unit -> unit) -> unit
end

module Make () : S

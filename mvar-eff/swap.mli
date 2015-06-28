module type S = sig
  type ('a,'b) endpoint

  type ('a,'b) reagent

  val mk_chan : unit -> ('a,'b) endpoint * ('b,'a) endpoint
  val swap    : ('a,'b) endpoint -> ('a,'b) reagent

  val (>>) : ('a,'b) reagent -> ('b,'c) reagent -> ('a,'c) reagent
  val (+) : ('a,'b) reagent -> ('a,'b) reagent -> ('a,'b) reagent
  val (!) : ('a,'b) reagent -> 'a -> 'b
end

module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : 'a cont * 'a -> unit
end

module Make (Sched : SCHED) : S

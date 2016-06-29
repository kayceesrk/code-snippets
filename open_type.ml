module type SCHED = sig
  type 'a cont
  type 'a effect = ..
  type 'a effect += Resume  : 'a cont * 'a -> unit effect
  type 'a effect += Suspend : ('a cont -> unit) -> 'a effect
end

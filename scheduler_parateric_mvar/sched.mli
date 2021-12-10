open EffectHandlers

type 'a resumer = 'a -> unit
type 'a suspender = ('a resumer -> unit) -> 'a
type _ eff += Stuck : unit eff

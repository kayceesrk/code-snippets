open Obj.Effect_handlers

type _ eff += Fork : (unit -> 'a) -> 'a list eff

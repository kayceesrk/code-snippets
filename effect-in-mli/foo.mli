open Obj.Effect_handlers

module Promise : sig
  type !'a t
end

type _ eff += Fork : (unit -> 'a) -> 'a Promise.t eff

open Obj.Effect_handlers

module Promise = struct 
  type 'a t = 'a
end 

type _ eff += Fork : (unit -> 'a) -> 'a Promise.t eff

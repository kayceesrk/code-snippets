open Sched_monad

type 'a promise = 'a MVar_monad.t

let async f =
  let m = MVar_monad.create_empty () in
  fork (f >>= MVar_monad.put m) >>= fun () ->
  return m

let await m = MVar_monad.take m

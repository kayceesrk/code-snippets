let n = try int_of_string (Sys.argv.(1)) with _ -> 25

open Sched_monad

type 'a tree =
| Leaf
| Node of 'a tree * 'a * 'a tree

let rec make = function
  | 0 -> Leaf
  | n -> let t = make (n-1) in Node (t,n,t)

let rec iter (m : 'a option MVar_monad.t) (t : 'a tree) =
  match t with
  | Leaf -> return ()
  | Node (l, x, r) ->
      iter m l >>= fun _ ->
      MVar_monad.put m (Some x) >>= fun _ ->
      iter m r

let main () =
  let t = make n in
  let m = MVar_monad.create_empty () in
  let rec consume () =
    MVar_monad.take m >>= fun v ->
    match v with
    | None -> return ()
    | Some _ -> consume ()
  in
  run (
    fork (iter m t >>= fun _ -> MVar_monad.put m None) >>= fun _ ->
    consume ())

let _ = main ()

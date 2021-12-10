open EffectHandlers
open EffectHandlers.Deep

module F = Fifo_sched.Make ()
module L = Lifo_sched.Make ()

let m = MVar.create_empty ()

let main () =
  let comp () =
    F.run (fun () ->
    L.run (fun () ->
      L.fork (fun _ ->
        Printf.printf "Thread in Lifo scheduler taking from MVar\n";
        let v = MVar.take m in
        Printf.printf "Thread in Lifo scheduler took %d from MVar\n" v);
      F.fork (fun _ ->
        let v = 42 in
        Printf.printf "Thread in Fifo scheduler putting %d into MVar\n" v;
        MVar.put v m)))
  in
  match_with comp ()
  { retc = (fun () -> ());
    exnc = (function
      | Exit -> ()
      | e -> Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
    effc = fun (type a) (e : a eff) ->
      match e with
      | Sched.Stuck -> Some (fun (k : (a,_) continuation) ->
          discontinue k Exit)
      | e -> None }

let _ = main ()

open Printf
open EffectHandlers
open EffectHandlers.Deep

module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val suspend : 'a Sched.suspender
  val run : (unit -> unit) -> unit
end

module Make () : S = struct

  type _ eff += Fork  : (unit -> unit) -> unit eff
  type _ eff += Yield : unit eff

  type 'a cont = ('a,unit) continuation

  type _ eff += Suspend : ('a cont -> unit) -> 'a eff
  type _ eff += Resume  : ('a cont * 'a) -> unit eff

  let run_q = Queue.create ()

  let enqueue t v =
    Queue.push (fun () -> continue t v) run_q

  let rec dequeue () =
    if Queue.is_empty run_q then perform Sched.Stuck
    else Queue.pop run_q ()

  let run main =

    let rec spawn f =
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a eff) ->
          match e with
          | Yield -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); dequeue ())
          | Fork f -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); spawn f)
          | Suspend f -> Some (fun (k: (a,_) continuation) ->
              f k; dequeue ())
          | Resume (k', v) -> Some (fun (k: (a,_) continuation) ->
              enqueue k' v; continue k ())
          | Sched.Stuck -> Some (fun (k: (a, _) continuation) ->
              if Queue.is_empty run_q then
                try ignore (discontinue k Exit) with _ -> ()
              else begin
                enqueue k (); dequeue ()
              end)
          | e -> None }
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield

  let suspend f =
    perform (Suspend (fun k ->
    let resumer v = enqueue k v in
    f resumer))
end

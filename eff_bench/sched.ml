open Effect.Deep

type _ Effect.t += Fork    : (unit -> unit) -> unit Effect.t
                 | Yield   : unit Effect.t

type 'a cont = ('a,unit) continuation
type _ Effect.t += Suspend : ('a cont -> unit) -> 'a Effect.t
type _ Effect.t += Resume  : ('a cont * 'a) -> unit Effect.t

let run main =
  let run_q = Queue.create () in
  let enqueue t v =
    Queue.push (fun () -> continue t v) run_q
  in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else Queue.pop run_q ()
  in
  let rec spawn f =
    match_with f ()
    { retc = dequeue;
      exnc = raise;
      effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Yield -> Some (fun (k: (a,_) continuation) ->
            enqueue k (); dequeue ())
        | Fork f -> Some (fun (k: (a,_) continuation) ->
            enqueue k (); spawn f)
        | Suspend f -> Some (fun (k: (a,_) continuation) ->
            f k; dequeue ())
        | Resume (k',v) -> Some (fun (k: (a,_) continuation) ->
            enqueue k' v; continue k ())
        | _ -> None}
  in
  spawn main

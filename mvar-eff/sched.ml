open Effects
open Printf

type thread_id = int
type 'a cont = Cont : ('a,unit) continuation * thread_id -> 'a cont

type 'a effect = ..

type _ Effects.effect +=
  | Fork    : (unit -> unit) -> unit Effects.effect
  | Yield   : unit Effects.effect
  | Suspend : ('a cont -> unit) -> 'a Effects.effect
  | Resume  : 'a cont * 'a -> unit Effects.effect
  | Get_Tid : int Effects.effect

let fork f = perform (Fork f)
let yield () = perform Yield

let run main =
  (* Thread ID *)
  let cur_tid = ref (-1) in
  let next_tid = ref 0 in
  (* Run queue handling *)
  let run_q = Queue.create () in
  let enqueue t v tid =
    Queue.push (fun () -> (cur_tid := tid; continue t v)) run_q
  in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else Queue.pop run_q ()
  in
  let rec spawn : type a . (a -> unit) -> a -> unit =
    fun f x ->
      cur_tid := !next_tid;
      next_tid := !next_tid + 1;
      handle scheduler f x
    and scheduler =
      {return = dequeue;
      exn = raise;
      effect = fun (type a) (eff : a Effects.effect) (k : (a, unit) continuation) ->
        match eff with
        | Yield ->
            enqueue k () !cur_tid;
            dequeue ()
        | Fork f ->
            enqueue k () !cur_tid;
            spawn f ()
        | Suspend f ->
            f (Cont (k,!cur_tid));
            dequeue ()
        | Resume(Cont (k',tid), v) ->
            enqueue k' v tid;
            continue k ()
        | Get_Tid -> continue k !cur_tid
        | _ -> delegate eff k}
  in
  spawn main ()


let suspend f = perform (Suspend f)
let resume (k,v) = perform (Resume (k,v))
let get_tid () = perform Get_Tid

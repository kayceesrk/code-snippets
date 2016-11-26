module type Handler = sig
  type effect = ..
  type 'a t
  type cont

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val run    : unit t -> unit
  val handle : 'a t -> (effect -> cont -> unit t) -> unit t
  val continue : cont -> unit t
  val perform : effect -> unit t -> unit t
end

module Handler : Handler = struct
  type effect = ..

  type action =
    | Perform of effect * cont
    | Done

  and 'a t = ('a -> action) -> action

  and cont = unit t

  let (>>=) f k = fun k' -> f (fun a -> k a k')
  let return x = fun k -> k x
  let perform e k = fun _ -> Perform (e, k)
  let continue k = fun k' -> k k'

  let handle (e : 'a t) h = fun (k : unit -> action) ->
    match e (fun _ -> Done) with
    | Done -> k ()
    | Perform (eff, cont) -> h eff cont k

  let run p =
    match p (fun _ -> Done) with
    | Done -> ()
    | Perform _ -> failwith "Unhandled"
end

open Handler

type effect += E

let _ = run @@
  handle (perform E (return ())) (fun eff k ->
    match eff with
    | E ->
        Printf.printf "Handling E\n";
        continue k >>= fun () ->
        Printf.printf "After continue\n";
        return ()
    | _ ->
        Printf.printf "Delegating\n";
        perform eff (continue k))

module type Sched = sig
  val fork : unit t -> unit t -> unit t
  val yield : unit t -> unit t
  val run : unit t -> unit
end

module Sched : Sched = struct
  type effect +=
  | Fork of unit t
  | Yield

  let fork f k = perform (Fork f) k

  let yield k = perform (Yield) k

  (* A concurrent round-robin scheduler *)
  let run main =
    let run_q = Queue.create () in
    let enqueue k = Queue.push k run_q in
    let rec dequeue () =
      if Queue.is_empty run_q then return ()
      else continue (Queue.pop run_q)
    in
    let rec spawn f =
      handle f handler
    and handler eff k =
      match eff with
      | Yield -> enqueue k; dequeue ()
      | Fork f -> enqueue k; handle (spawn f) handler
      | _ -> perform eff (continue k)
    in
    run @@ spawn main
end


let log = Printf.printf

let rec f id depth =
  log "Starting number %i\n%!" id;
  if depth > 0 then begin
    log "Forking number %i\n%!" (id * 2 + 1);
    Sched.fork (f (id * 2 + 1) (depth - 1)) (
    log "Forking number %i\n%!" (id * 2 + 2);
    Sched.fork (f (id * 2 + 2) (depth - 1)) (return ()))
  end else begin
    log "Yielding in number %i\n%!" id;
    Sched.yield (
    log "Resumed number %i\n%!" id;
    return ())
  end;
  log "Finishing number %i\n%!" id;
  return ()

let () = Sched.run (f 0 2)

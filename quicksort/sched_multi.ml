
(* Naive work-stealing parallel scheduler *)

module Mutex = struct
  type mutex = int Kcas.ref
  let mutex () = Kcas.ref 0
  let lock r =
    let rec lock_core r b = 
      match Kcas.map r (fun i -> if i = 0 then Some 1 else None) with
      | Success _ -> ()
      | Failed -> failwith "Impossible"
      | Aborted -> 
          match b with
          | None -> lock_core r (Some (Backoff.create ()))
          | Some b' -> Backoff.once b'; lock_core r b
    in
    lock_core r None
         
  let unlock r = 
    match Kcas.map r (fun _ -> Some 0) with
    | Success _ -> ()
    | _ -> failwith "Impossible"
end

module State = struct

  type 'a queue =
    { mutex: Mutex.mutex;
      mutable values: 'a list; }

  type 'a t =
    { domains : int;
      stealing_mutex : Mutex.mutex;
      mutable stealing : int;
      queues : 'a queue array; }

  let create domains =
    let stealing_mutex = Mutex.mutex () in
    let stealing = 0 in
    let queues =
      Array.init domains
        (fun _ ->
           { mutex = Mutex.mutex ();
             values = []; })
    in
      { domains; stealing_mutex; stealing; queues }

  let push state self k =
    Mutex.lock state.queues.(self).mutex;
    state.queues.(self).values <- k :: state.queues.(self).values;
    Mutex.unlock state.queues.(self).mutex

  let pop state self =
    Mutex.lock state.queues.(self).mutex;
    let k =
      match state.queues.(self).values with
      | k :: ks ->
        state.queues.(self).values <- ks;
        Some k
      | [] ->
        Mutex.lock state.stealing_mutex;
        state.stealing <- state.stealing + 1;
        Mutex.unlock state.stealing_mutex;
        None
    in
    Mutex.unlock state.queues.(self).mutex;
    k

  let finished state =
    Mutex.lock state.stealing_mutex;
    let res = state.stealing = state.domains in
    Mutex.unlock state.stealing_mutex;
    res

  let steal state self =
    let res = ref None in
    begin
      try
        while not (finished state) do
          for i = 0 to state.domains - 1 do
            if i <> self then begin
              Mutex.lock state.queues.(i).mutex;
              match List.rev state.queues.(i).values with
              | k :: ks ->
                state.queues.(i).values <- List.rev ks;
                Mutex.lock state.stealing_mutex;
                state.stealing <- state.stealing - 1;
                Mutex.unlock state.stealing_mutex;
                res := Some k;
                Mutex.unlock state.queues.(i).mutex;
                raise Exit;
              | [] ->
                Mutex.unlock state.queues.(i).mutex;
            end
          done;
        done;
      with Exit -> ()
    end;
    !res
end


effect Fork : (unit -> unit) -> unit
effect Yield : unit

let enqueue state k =
  let self = Domain.self () in
  State.push state self k

let dequeue state =
  let self = Domain.self () in
  match State.pop state self with
  | Some k -> continue k ()
  | None ->
      match State.steal state self with
      | Some k ->
          continue k ();
      | None -> ()

let rec spawn state f =
  match f () with
  | () -> dequeue state
  | effect Yield k ->
      enqueue state k;
      dequeue state
  | effect (Fork f) k ->
      enqueue state k;
      spawn state f

let domains = 1

let run main =
  let state = State.create domains in
  Gc.minor ();
  for i = 1 to domains - 1 do
    Domain.spawn (fun () -> dequeue state)
  done;
  spawn state main

let fork f = perform (Fork f)

let yield () = perform Yield

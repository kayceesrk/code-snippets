module type S = sig
  type ('a,'b) endpoint

  type ('a,'b) reagent

  val mk_chan : unit -> ('a,'b) endpoint * ('b,'a) endpoint
  val swap    : ('a,'b) endpoint -> ('a,'b) reagent

  val (>>) : ('a,'b) reagent -> ('b,'c) reagent -> ('a,'c) reagent
  val (+) : ('a,'b) reagent -> ('a,'b) reagent -> ('a,'b) reagent
  val (!) : ('a,'b) reagent -> 'a -> 'b
end

module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : 'a cont * 'a -> unit
  effect Get_Tid  : int
end

module Make (Sched : SCHED) : S = struct

  module Offer = struct

    type 'a status =
    | Pending of 'a option Sched.cont
    | Fulfilled

    type 'a t = 'a status ref

    let make_offer k =
      Printf.printf "make_offer\n";
      ref (Pending k)

    let is_pending o =
      match !o with
      | Pending _ -> true
      | _ -> false

  end

  module Reaction = struct
    type react =
      {is_pending : unit -> bool;
       commit     : unit -> unit;
       abort      : unit -> unit}

    type t = react list

    let add r o v =
      {is_pending = (fun () -> Offer.is_pending o);
       commit = (fun () ->
         match !o with
         | Offer.Fulfilled -> failwith "Reaction.add.fulfil"
         | Offer.Pending k -> perform @@ Sched.Resume (k, Some v));
       abort = (fun () ->
         match !o with
         | Offer.Fulfilled -> ()
         | Offer.Pending k -> perform @@ Sched.Resume (k, None))}::r

    let append r1 r2 = r1 @ r2

    exception CommitFail

    let rec ensurePending = function
    | [] -> ()
    | {is_pending; _}::xs ->
        if is_pending () then ensurePending xs else raise CommitFail

    let tryCommit r =
      try
        ensurePending r;
        List.iter (fun {commit; _} -> commit ()) r;
        true
      with
      | CommitFail ->
          List.iter (fun {abort; _} -> abort ()) r;
          false

    let empty = []
  end

  type 'a result = Block | Retry | Done of 'a

  type ('a,'b) reagent =
    { tryReact : 'a -> Reaction.t -> 'b Offer.t option -> 'b result * Reaction.t;
      seq : 'c. ('b,'c) reagent -> ('a,'c) reagent }

  type ('a,'b) message =
    Message : 'a * Reaction.t * ('b,'r) reagent * 'r Offer.t -> ('a,'b) message

  type ('a,'b) endpoint =
    {outgoing: ('a,'b) message Queue.t;
     incoming: ('b,'a) message Queue.t}

  let mk_chan () =
    let l1 = Queue.create () in
    let l2 = Queue.create () in
    {incoming = l1; outgoing = l2},
    {incoming = l2; outgoing = l1}

  let qid q : int =
    Obj.magic (Obj.repr q)

  let rec clean q =
    try
      let Message (_,_,_,offer) = Queue.peek q in
      if Offer.is_pending offer
      then ()
      else (ignore (Queue.pop q); clean q)
    with
    | Queue.Empty -> ()

  let clean_and_pop q =
    clean q;
    try Some (Queue.pop q) with Queue.Empty -> None

  let get_tid () = perform @@ Sched.Get_Tid
  let (!) r v =
    let rec withoutOffer () =
      match r.tryReact v Reaction.empty None with
      | (Done res, rx) -> res
      | _ -> withOffer ()
      (* XXX TODO Retry *)
    and withOffer () =
      match
        perform @@ Sched.Suspend (fun k ->
          let offer = Offer.make_offer k in
          ignore (r.tryReact v Reaction.empty (Some offer)))
      with
      | Some res -> res
      | None -> withOffer ()
    in withoutOffer ()

  let rec swap_internal : 'a 'b 'r. ('a,'b) endpoint -> ('b,'r) reagent -> ('a,'r) reagent  =
    fun ep k ->
      let {outgoing; incoming} = ep in
      let () = (clean incoming; clean outgoing) in
      let seq next = swap_internal ep (k.seq next) in
      let swapK dualPayload dualOffer =
        {seq = (fun _ -> failwith "swapK: impossible");
         tryReact = fun s rx my_offer ->
           Printf.printf "swapK: tryReact\n";
           k.tryReact dualPayload (Reaction.add rx dualOffer s) my_offer}
      in
      let tryReact a rx offer =
        let () = Printf.printf "incoming (%x) = %d outgoing (%x) = %d\n"
                  (qid incoming) (Queue.length incoming)
                  (qid outgoing) (Queue.length outgoing)
        in
        let () =
          match offer with
          | None -> Printf.printf "swap: without offer\n"
          | Some offer ->
              Printf.printf "swap: with offer\n";
              Queue.push (Message (a,rx,k,offer)) outgoing;
              Printf.printf "After push : incoming (%x) = %d outgoing (%x) = %d\n"
                  (qid incoming) (Queue.length incoming)
                  (qid outgoing) (Queue.length outgoing)
        in
        let rec tryFrom q fail_mode =
          match clean_and_pop q with
          | None ->
              Printf.printf "swap: no match\n";
              (fail_mode, rx)
          | Some (Message (a',rx',k',offer') as m) ->
              Printf.printf "swap: match found\n";
              let merged = k'.seq (swapK a' offer') in
              let new_rx = Reaction.append rx rx' in
              match merged.tryReact a new_rx offer with
              | (Retry, _)  -> tryFrom q Retry
              | (Block, _) -> tryFrom q fail_mode
              | _ as r -> r

        in
        tryFrom (Queue.copy incoming) Block
      in
      {seq; tryReact}

  let (>>) r1 r2 = r1.seq r2

  let commit_reagent =
    { seq = (fun k -> k);
      tryReact = fun arg rx _ ->
        Printf.printf "commit_reagent\n";
        if Reaction.tryCommit rx
        then (Done arg, rx)
        else failwith "commit_reagent: should retry"}

  let swap ep = swap_internal ep commit_reagent

  let rec (+) : 'a 'b. ('a,'b) reagent -> ('a,'b) reagent -> ('a,'b) reagent =
    fun r1 r2 ->
      {seq = (fun next -> (r1.seq next) + (r2.seq next));
      tryReact = fun a rx offer ->
        match r1.tryReact a rx offer with
        | (Block,_) -> r2.tryReact a rx offer
        | (Retry,_) ->
            begin
              match r2.tryReact a rx offer with
              | (Done _, _) as v -> v
              | (Block,rx) | (Retry,rx) -> (Retry, rx)
            end
        | _ as v -> v}
end

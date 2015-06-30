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
end

let debug f = () (* f () *)

module Make (Sched : SCHED) : S = struct

  module Offer = struct

    type offer_id = int

    let id = ref 0

    let get_next_id () =
      id := !id + 1;
      !id - 1

    type 'a status =
      | Pending of 'a option Sched.cont
      | Fulfilled

    type 'a t = offer_id * 'a status ref

    let make_offer k =
      (get_next_id (), ref (Pending k))

    let is_pending (_,offer) =
      match !offer with
      | Pending _ -> true
      | _ -> false

    let commit (_,offer) v =
      match !offer with
      | Pending k -> perform @@ Sched.Resume (k, Some v)
      | _ -> failwith "Offer.commit"

    let abort (_,offer) =
      match !offer with
      | Pending k -> perform @@ Sched.Resume (k, None)
      | _ -> failwith "Offer.abort"

    let get_id (i,_) = i

    let mk_dummy_offer i = (i, ref Fulfilled)

    let same_offer (i,_) (j,_) = i = j
  end

  module Reaction = struct

    type rx = Rx : 'a Offer.t * 'a -> rx

    module Rx = struct
      type t = Offer.offer_id * rx
      let compare (i1,_) (i2,_) = compare i1 i2
    end

    module RxSet = Set.Make (Rx)

    type t = RxSet.t

    let append = RxSet.union

    exception CommitFail

    let add r o v = RxSet.add (Offer.get_id o, Rx (o,v)) r

    let ensurePending r =
      RxSet.iter (fun (_,Rx (offer, _)) ->
        if Offer.is_pending offer then () else raise CommitFail) r

    let tryCommit r =
      try
        ensurePending r;
        RxSet.iter (fun (_,Rx (offer, v)) -> Offer.commit offer v) r;
        true
      with
      | CommitFail ->
          RxSet.iter (fun (_,Rx (offer, _)) -> Offer.abort offer) r;
          false

    let empty = RxSet.empty

    let has_offer r (i,_) = RxSet.mem (i, Rx (Offer.mk_dummy_offer i, 0)) r
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
          match r.tryReact v Reaction.empty (Some offer) with
          | Block,_ -> debug (fun () -> Printf.printf "withOffer.Block\n")
          | Retry,_ -> debug (fun () -> Printf.printf "withOffer.Retry\n")
          | Done _,_ -> debug (fun () -> Printf.printf "withOffer.Done\n"))
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
           debug (fun () -> Printf.printf "swapK: tryReact\n");
           k.tryReact dualPayload (Reaction.add rx dualOffer s) my_offer}
      in
      let tryReact a rx offer =
        let () = debug (fun () ->
                   Printf.printf "incoming (%x) = %d outgoing (%x) = %d\n"
                    (qid incoming) (Queue.length incoming)
                    (qid outgoing) (Queue.length outgoing))
        in
        let () =
          match offer with
          | None -> debug (fun () -> Printf.printf "swap: without offer\n")
          | Some offer ->
              debug (fun () -> Printf.printf "swap: with offer\n");
              Queue.push (Message (a,rx,k,offer)) outgoing;
              debug (fun () ->
                Printf.printf "After push : incoming (%x) = %d outgoing (%x) = %d\n"
                  (qid incoming) (Queue.length incoming)
                  (qid outgoing) (Queue.length outgoing))
        in
        let rec tryFrom q fail_mode =
          match clean_and_pop q with
          | None ->
              debug (fun () -> Printf.printf "swap: no match\n");
              (fail_mode, rx)
          | Some (Message (a',rx',k',offer')) ->
              let new_rx = Reaction.append rx rx' in
              let same_offer () =
                match offer with
                | None -> false
                | Some offer -> Offer.same_offer offer offer'
              in
              if Reaction.has_offer new_rx offer' || same_offer () then
                (debug (fun () -> Printf.printf "swap: unsuitable offer\n");
                 tryFrom q fail_mode)
              else
                (debug (fun () -> Printf.printf "swap: match found offer'=%d\n" (Offer.get_id offer'));
                 let merged = k'.seq (swapK a' offer') in
                 match merged.tryReact a new_rx offer with
                 | (Retry, _)  -> tryFrom q Retry
                 | (Block, _) -> tryFrom q fail_mode
                 | _ as r -> r)
        in
        tryFrom (Queue.copy incoming) Block
      in
      {seq; tryReact}

  let (>>) r1 r2 = r1.seq r2

  let commit_reagent =
    { seq = (fun k -> k);
      tryReact = fun arg rx _ ->
        debug (fun () -> Printf.printf "commit_reagent\n");
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

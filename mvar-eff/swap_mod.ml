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

module Make (Sched : SCHED) : S = struct

  module Offer = struct

    type 'a status =
    | Pending of 'a option Sched.cont
    | Fulfilled

    type 'a t = 'a status ref

    let make_offer k = ref (Pending k)

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

  type ('a,'b) reagent =
    { tryReact : 'a -> Reaction.t -> 'b Offer.t option -> 'b option * Reaction.t;
      seq : 'c. ('b,'c) reagent -> ('a,'c) reagent }

  type ('b,'r) message_cont =
    { senderK : ('b,'r) reagent;
      offer   : 'r Offer.t }

  type ('a,'b) message =
    { payload  : 'a;
      senderRx : Reaction.t;
      cont     : 'r. ('b,'r) message_cont }

  type ('a,'b) endpoint =
    {outgoing: ('a,'b) message Queue.t;
     incoming: ('b,'a) message Queue.t}

  let mk_chan () =
    let l1 = Queue.create () in
    let l2 = Queue.create () in
    {incoming = l1; outgoing = l2},
    {incoming = l2; outgoing = l1}

  let rec clean q =
    try
      let {cont = {offer;_}; _} = Queue.peek q in
      if Offer.is_pending offer
      then q
      else (ignore (Queue.pop q); clean q)
    with
    | Queue.Empty -> q

  let clean_and_pop q =
    clean q;
    try Some (Queue.pop q) with Queue.Empty -> None

  let (!) r v =
    let rec withoutOffer () =
      match r.tryReact v Reaction.empty None with
      | (Some res, rx) ->
          if Reaction.tryCommit rx then res else withoutOffer ()
      | _ -> withOffer ()
    and withOffer () =
      match
        perform @@ Sched.Suspend (fun k ->
          let offer = Offer.make_offer k in
          ignore (r.tryReact v Reaction.empty (Some offer)))
      with
      | Some res -> res
      | None -> withOffer ()
    in withoutOffer ()

  let swap_internal (type a) (type b) (type r) (ep : (a,b) endpoint) (k : (b,r) reagent) =
    let {outgoing; incoming} = ep in
    let seq next = k.seq next in
    let tryReact a rx offer =
      match offer with
      | None -> ()
      | Some offer ->
          let cont : (b,r) message_cont = {senderK = k; offer} in
          Queue.push outgoing {payload = a; senderRx = rx; cont};
      failwith "not implemented"
    in
    {seq; tryReact}

  let noop_reagent =
    { seq = (fun k -> k);
      tryReact = fun arg rx _ -> (Some arg, rx) }

  let swap ep = swap_internal ep noop_reagent
end

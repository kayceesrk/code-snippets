module Offer = struct

  type offer_id = int

  let id = ref 0

  let get_next_id () =
    id := !id + 1;
    !id - 1

  type 'a status =
    | Pending of 'a option ref
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
    | Pending r -> r := (Some v)
    | _ -> failwith "Offer.commit"

  let abort (_,offer) =
    match !offer with
    | Pending r -> r := None
    | _ -> failwith "Offer.abort"

  let get_id (i,_) = i

  let mk_dummy_offer i = (i, ref Fulfilled)
end

module Reaction = struct

  type rx = Rx : 'a Offer.t * 'a -> rx

  module Rx = struct
    type t = Offer.offer_id * rx
    let compare (i1,_) (i2,_) = compare i1 i2
  end

  module RxSet = Set.Make (Rx)

  type t = RxSet.t

  let (++) = RxSet.union

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

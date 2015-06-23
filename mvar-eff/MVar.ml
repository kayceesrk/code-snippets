module type S = sig
  type 'a t
  val new_mvar       : 'a -> 'a t
  val new_empty_mvar : unit -> 'a t
  val put_mvar       : 'a -> 'a t -> unit
  val take_mvar      : 'a t -> 'a

  type ('a,'b) reagent

  val take_mvar_evt : 'a t -> ('a -> 'b -> 'c) -> ('b, 'c) reagent
  val put_mvar_evt  : 'a t -> ('a -> 'b -> 'c) -> ('a * 'b, 'c) reagent

  val (>>) : ('a,'b) reagent -> ('b,'c) reagent -> ('a,'c) reagent
  val (+) : ('a,'b) reagent -> ('a,'b) reagent -> ('a,'b) reagent
  val (!) : ('a,'b) reagent -> 'a -> 'b
end

module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : 'a cont * 'a -> unit
end

module Make (S : SCHED) : S = struct

  type 'a cont =
    | Fun of ('a -> unit)
    | Stack of 'a S.cont

  type 'a offer = 'a cont option ref

  (** The state of mvar is either [Full v q] filled with value [v] and a queue
      [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
      threads waiting to empty the mvar. *)
  type 'a mv_state =
    | Full  of 'a * ('a * unit offer) Queue.t
    | Empty of 'a offer Queue.t

  type 'a t = 'a mv_state ref

  let new_empty_mvar () = ref (Empty (Queue.create ()))

  let new_mvar v = ref (Full (v, Queue.create ()))

  let suspend f = perform @@ S.Suspend f

  let resume v = function
  | Fun f -> f v
  | Stack k -> perform @@ S.Resume (k,v)

  let rec put_mvar v mv =
    match !mv with
    | Full (v', q) -> suspend (fun k -> Queue.push (v,ref(Some(Stack k))) q)
    | Empty q ->
        if Queue.is_empty q then
          mv := Full (v, Queue.create ())
        else
          let t = Queue.pop q in
          match !t with
          | None -> put_mvar v mv
          | Some k -> t := None; resume v k

  let take_mvar mv =
    let rec clean q =
      if Queue.is_empty q then
        mv := Empty (Queue.create ())
      else
        let (v', t) = Queue.pop q in
        match !t with
        | None -> clean q
        | Some k -> mv := Full (v', q); resume () k
    in
    match !mv with
    | Empty q -> suspend (fun k -> Queue.push (ref(Some(Stack k))) q)
    | Full (v, q) -> clean q; v

  type ('a,'b) poll_result =
  | Block of ('a -> 'b offer -> 'b)
  | Do of ('a -> 'b)

  type ('a,'b) reagent = unit -> ('a,'b) poll_result

  let (+) r1 r2 = fun () ->
    match r1 () with
    | Do df1 -> Do df1
    | Block bf1 ->
        match r2 () with
        | Do df2 -> Do df2
        | Block bf2 ->
            Block (fun a o -> bf1 a (ref (Some (Fun (fun b -> bf2 b o)))))

  let (>>) (r1 : ('a,'b) reagent) (r2 : ('b,'c) reagent) : ('a, 'c) reagent = fun () ->
    match r1 () with
    | Do df1 ->
        begin
          match r2 () with
          | Do df2 -> Do (fun a -> df2 (df1 a))
          | Block bf2 -> Block (fun a o -> bf2 (df1 a) o)
        end
    | Block bf1 -> Block (fun a o -> bf1 a (ref (Some (Fun (fun b ->
        match r2 () with
        | Do df2 ->
            begin
              match !o with
              | Some k -> resume (df2 b) k
              | None -> failwith ">> : impossible"
            end
        | Block bf2 -> bf2 b o)))))

  let (!) r v =
    match r () with
    | Do d -> d v
    | Block b -> suspend (fun k -> b v (ref @@ Some (Stack k)))

  let take_mvar_evt = failwith "not implemented"

  let put_mvar_evt = failwith "not implemented"

end

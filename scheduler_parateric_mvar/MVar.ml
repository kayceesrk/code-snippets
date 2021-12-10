open EffectHandlers

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Queue.t
  | Empty of 'a Sched.resumer Queue.t

type 'a t = 'a mv_state ref

let create_empty () = ref (Empty (Queue.create ()))

let create v = ref (Full (v, Queue.create ()))

let put v mv =
  match !mv with
  | Full (v', q) -> perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
  | Empty q ->
      if Queue.is_empty q then
        mv := Full (v, Queue.create ())
      else
        let resume = Queue.pop q in
        resume v

let take mv =
  match !mv with
  | Empty q -> perform (Sched.Suspend (fun r -> Queue.push r q))
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        resume ();
        v
      end

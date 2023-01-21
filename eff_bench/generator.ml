open Effect
open Effect.Deep

let n = try int_of_string (Sys.argv.(1)) with _ -> 25

module MkGen (S :sig
  type 'a t
  val iter : ('a -> unit) -> 'a t -> unit
end) : sig
  val gen : 'a S.t -> (unit -> 'a option)
end = struct
  let gen : type a. a S.t -> (unit -> a option) = fun l ->
    let module M = struct
      type _ Effect.t += Next : a -> unit Effect.t
    end in
    let open M in
    let rec step = ref (fun () ->
      try_with
       (fun t -> S.iter (fun x -> perform (Next x)) t; None)
       l
       { effc = fun (type a) (e : a Effect.t) ->
         match e with
        | Next v ->
            Some (fun (k : (a, _) continuation) ->
              step := (fun () -> continue k ());
            Some v)
        | _ -> None })
    in
    fun () -> !step ()
end

(* A generator for a list *)
module L = MkGen(struct
  type 'a t = 'a list
  let iter = List.iter
end)

type 'a tree =
| Leaf
| Node of 'a tree * 'a * 'a tree

let rec make = function
  | 0 -> Leaf
  | n -> let t = make (n-1) in Node (t,n,t)

let rec iter f = function
  | Leaf -> ()
  | Node (l, x, r) -> iter f l; f x; iter f r

(* A generator for a tree *)
module T = MkGen(struct
  type 'a t = 'a tree
  let iter = iter
end)

let main () =
  let next = T.gen (make n) in
  let rec consume () =
    match next () with
    | None -> ()
    | Some _ -> consume ()
  in
  consume ()

let _ = main ()

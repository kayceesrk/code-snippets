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
    | Stop

  and 'a t = ('a -> action) -> action

  and cont = unit t

  let (>>=) f k = fun k' -> f (fun a -> k a k')
  let return x = fun k -> k x
  let perform e k = fun _ -> Perform (e, k)
  let continue k = fun k' -> k k'

  let rec handle (e : 'a t) h = fun (k : unit -> action) ->
    match e (fun _ -> Stop) with
    | Stop -> k ()
    | Perform (eff, cont) -> h eff cont k

  let run p =
    match p (fun _ -> Stop) with
    | Stop -> ()
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

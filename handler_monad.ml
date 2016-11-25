module type Handler = sig
  type effect = ..
  type 'a t
  type 'a cont

  val (>>=)    : 'a t -> ('a -> 'b t) -> 'b t
  val return   : 'a -> 'a t
  val perform  : effect -> unit t
  val handle   : 'a t -> (effect -> unit cont -> 'a t) -> 'a t
  val continue : 'a cont -> 'a -> 'a t
  val run      : 'a t -> unit
end

module Handler : Handler = struct
  type effect = ..

  type 'a cont = 'a -> action
  and action =
    | Perform of effect * unit cont
    | Stop

  type 'a t = 'a cont -> action

  let return x = fun k -> k x

  let (>>=) (f : 'a t) (k : 'a -> 'b t) = fun c -> f (fun a -> k a c)

  let handle expr h = fun k ->
    match expr (fun _ -> Stop) with
    | Perform (eff, k') -> h eff k' k
    | Stop -> failwith "STOP"

  let continue k v = fun k' -> k v

  let perform e = fun k -> Perform (e, k)

  let run p =
    match p (fun _ -> Stop) with
    | Perform _ -> failwith "Unhandled"
    | Stop -> ()
end

open Handler

type effect += E

let _ = run (handle (perform E) (fun eff k ->
          match eff with
          | E -> Printf.printf "Handling E\n"; (continue k () >>= fun () -> Printf.printf "finishing\n"; return ())
          | e -> failwith "Unhandled"))

open EffectHandlers

type _ eff += E : int eff

let ok () =
  let open Deep in
  match_with perform E
  { retc = string_of_int;
    exnc = raise;
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k:(a,string) continuation) -> continue k 42)
      | _ -> None}

(* Shallow continuations cannot be used as Deep continuations. This will print
   a random integer. *)
let barf1 () =
  let open Shallow in
  let f () = perform E in
  let k : (unit, int) continuation = fiber f in
  continue_with k ()
  { retc = string_of_int;
    exnc = Printexc.to_string;
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k:(a,int) continuation) ->
          let k': (a, int) Deep.continuation = Obj.magic k in
          string_of_int (Deep.continue k' 42))
      | _ -> None }

(* Deep continuations cannot be used as Shallow continuations. This will
   segfault. *)
let barf2 () =
  let open Deep in
  match_with perform E
  { retc = (fun i -> string_of_int i);
    exnc = raise;
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k:(a,string) continuation) ->
          let k' : (a,string) Shallow.continuation = Obj.magic k in
          Shallow.continue_with k' 42
          { retc = Fun.id;
            exnc = raise;
            effc = fun e -> None })
      | _ -> None}

let _ =
  print_endline (ok ());
  print_endline (barf1 ());
  print_endline (barf2 ())

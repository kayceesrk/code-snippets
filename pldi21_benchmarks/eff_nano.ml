let n = try int_of_string Sys.argv.(1) with _ -> 10
effect E : unit

let foo () =
  nop (); (* a *)
  match
    nop (); (* b *)
    perform E;
    nop (); (* d *)
  with
  | () ->
      nop (); (* e *)
      ()
  | effect E k ->
      nop (); (* c *)
      continue k ()

let rec loop n =
  if n = 0 then ()
  else (foo ();loop (n-1))

let _ = loop n

let n = try int_of_string Sys.argv.(1) with _ -> 10
exception E

let foo () =
  nop (); (* a *)
  try
    nop (); (* b *)
    raise E
  with
  | E ->
      nop () (* c *)

let rec loop n =
  if n = 0 then ()
  else (foo ();loop (n-1))

let _ = loop n

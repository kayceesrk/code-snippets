let n = try int_of_string (Sys.argv.(1)) with _ -> 1_000_000
external ocaml_to_c : unit -> unit = "ocaml_to_c"

let rec loop n =
  if n = 0 then ()
  else (ocaml_to_c (); loop (n-1))

let _ = loop n

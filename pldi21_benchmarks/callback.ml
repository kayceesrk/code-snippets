let n = try int_of_string (Sys.argv.(1)) with _ -> 1_000_000
external ocaml_to_c : int -> unit = "ocaml_to_c"

let c_to_ocaml () = ()
let _ = Callback.register "c_to_ocaml" c_to_ocaml

let _ = ocaml_to_c n

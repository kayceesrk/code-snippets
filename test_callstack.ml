open Printf
open Printexc

let bar () =
  printf "%s\n" (raw_backtrace_to_string (get_callstack 0))

let foo () =
  bar ();
  printf "completing foo"

let () = foo ()

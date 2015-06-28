module Swap = Swap.Make (Sched)

open Swap
open Sched
open Printf

let (ep1,ep2) = mk_chan ()

let get_tid () = perform Get_Tid
let fork f = perform @@ Fork f

let main () =
  Printf.printf "[%d] starting main\n" (get_tid ());
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep1 >> swap ep1) 0);
  fork (fun () -> printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2) 1);
  printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2) 2

let () = run main

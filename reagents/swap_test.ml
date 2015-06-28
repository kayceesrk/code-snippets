module Swap = Swap.Make (Sched)

open Swap
open Sched
open Printf

let get_tid () = perform Get_Tid
let fork f = perform @@ Fork f
let yield () = perform Yield

let main () =
  printf "[%d] starting main\n" (get_tid ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep1 >> swap ep1) 0);
  fork (fun () -> printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2) 1);
  printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2) 2;

  (* Test 2 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 2 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep1 + swap ep2) 0);
  printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2) 1;

  (* Test 3 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 3 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep1 >> swap ep1) 0);
  printf "will fail! Reagents are not as powerful as communicating transactions!\n";
  printf "[%d] %d\n%!" (get_tid ()) @@ (!) (swap ep2 >> swap ep2) 1;
  printf "should not see this!\n";

  ()

let () = run main

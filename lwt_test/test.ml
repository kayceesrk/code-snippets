let m = Lwt_mvar.create_empty ()

let putter () =
  Domain.spawn (fun () ->
    print_endline "putter started";
    ignore @@ Unix.sleep 2;
    print_endline "putter woken up";
    Lwt_main.run (Lwt_mvar.put m ());
    print_endline "putter terminating")

open Lwt.Infix

let my_lwt_main p =
  let term = Lwt_mvar.create_empty () in
  let rec sleep_loop () =
    Lwt.choose [Lwt_mvar.take term;
                Lwt_unix.sleep 0.1 >>= fun _ -> sleep_loop ()]
  in
  Lwt_main.run (
    Lwt.join [sleep_loop ();
              p >>= fun _ -> Lwt_mvar.put term ()])

let main () =
  ignore @@ putter ();
  ignore @@ Unix.select [] [] [] 1.0;
  print_endline "before Lwt_main.run";
  my_lwt_main (Lwt_mvar.take m);
  print_endline "after lwt_main.run"

let _ = main ()

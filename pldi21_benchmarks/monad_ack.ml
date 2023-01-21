(* compute the Ackermann function
 * (see Larcenry benchmarks http://www.larcenists.org/benchmarksAboutR6.html) *)

(* the usage of a new continuation is limited to the nested call *)

open Sched_monad

let rec ack m n =
  if m = 0 then return (n + 1)
  else if n = 0 then ack (m-1) 1
  else
    Async.async (ack m (n-1)) >>= fun p1 ->
    Async.await p1 >>= fun v1 ->
    ack (m-1) v1

let ack m n =
  let result = ref 0 in
  run (ack m n >>= fun v ->
       result := v;
       return ());
  !result

let rec repeat f acc n =
  if n = 1 then let x = f () in (Printf.printf "%d\n%!" x; x)
  else repeat f (acc + (f ())) (n-1)

let run f n = ignore (Sys.opaque_identity (repeat f 0 n))

let _ =
  let iters = try int_of_string Sys.argv.(1) with _ -> 2 in
  let m = try int_of_string Sys.argv.(2) with _ -> 3 in
  let n = try int_of_string Sys.argv.(3) with _ -> 11 in
  (* default output should be 16381 *)

  run (fun () -> ack m n) iters

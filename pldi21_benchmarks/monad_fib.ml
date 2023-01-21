open Sched_monad

let rec fib n =
  match n with
  | 0 -> return 0
  | 1 -> return 1
  | n -> begin
      Async.async (fib (n-1)) >>= fun p1 ->
      Async.async (fib (n-2)) >>= fun p2 ->
      Async.await p1 >>= fun v1 ->
      Async.await p2 >>= fun v2 ->
      return (v1 + v2)
  end

let fib n =
  let result = ref 0 in
  run (fib n >>= fun v ->
       result := v;
       return ());
  !result

let rec repeat f acc n =
  if n = 1 then let x = f () in (Printf.printf "%d\n%!" x; x)
  else repeat f (acc + (f ())) (n-1)

let run f n = ignore (Sys.opaque_identity (repeat f 0 n))

let _ =
  let iters = try int_of_string Sys.argv.(1) with _ -> 4 in
  let n = try int_of_string Sys.argv.(2) with _ -> 40 in
  (* default output should be 102334155 *)

  run (fun () -> fib n) iters

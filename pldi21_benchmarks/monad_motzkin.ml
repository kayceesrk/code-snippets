(*
   "the n'th Motzkin number is the number of different ways of drawing
   non-intersecting chords between n points on a circle
   (not necessarily touching every point by a chord)."
    https://en.wikipedia.org/wiki/Motzkin_number
    See here for input/outputs: https://oeis.org/A001006/list
*)

open Sched_monad

let rec sum f i stop acc =
	if i > stop then return acc
  else
    f i >>= fun v ->
    sum f (i+1) stop (acc + v)

let rec motz n =
	if n <= 1 then return 1
	else begin
		let limit = n - 2 in
		let product i =
      Async.async (motz i) >>= fun p1 ->
      Async.async (motz (limit -i)) >>= fun p2 ->
      Async.await p1 >>= fun v1 ->
      Async.await p2 >>= fun v2 ->
      return (v1 * v2)
    in
    Async.async (motz (n-1)) >>= fun p1 ->
    Async.async (sum product 0 limit 0) >>= fun p2 ->
    Async.await p1 >>= fun v1 ->
    Async.await p2 >>= fun v2->
    return (v1 + v2)
	end

let motz n =
  let result = ref 0 in
  run (motz n >>= fun v ->
       result := v;
       return ());
  !result


let rec repeat f acc n =
  if n = 1 then let x = f () in (Printf.printf "%d\n%!" x; x)
  else repeat f (acc + (f ())) (n-1)

let run f n = ignore (Sys.opaque_identity (repeat f 0 n))

let _ =
  let iters = try int_of_string Sys.argv.(1) with _ -> 4 in
  let n = try int_of_string Sys.argv.(2) with _ -> 21 in
  (* default output should be 142547559 *)

  run (fun () -> motz n) iters


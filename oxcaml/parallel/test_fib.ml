open Stdio

let rec fib parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> fib parallel (n - 1))
        (fun parallel -> fib parallel (n - 2))
    in
    a + b

let fib_sequential n =
  let monitor = Parallel.Monitor.create_root () in
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Parallel.Scheduler.Sequential.schedule scheduler ~monitor ~f:(fun parallel ->
    printf "%d\n" (fib parallel n))

let fib_parallel (module S : Parallel.Scheduler.S_async) nd (n:int) =
  let monitor = Parallel.Monitor.create_root () in
  let scheduler = S.create ~domains:nd () in
  S.schedule scheduler ~monitor ~f:(fun parallel ->
    printf "%d\n" (fib parallel n));
  S.stop scheduler


let process_command_line () =
  match Sys.argv.(1) with
  | "sequential" ->
    let n = int_of_string Sys.argv.(2) in
    fib_sequential n
  | "work_stealing" ->
    let nd = int_of_string Sys.argv.(2) in
    let n = int_of_string Sys.argv.(3) in
    fib_parallel (module Parallel_scheduler_work_stealing) nd n
  | "stack" ->
    let nd = int_of_string Sys.argv.(2) in
    let n = int_of_string Sys.argv.(3) in
    fib_parallel (module Parallel_scheduler_stack) nd n
  | _ -> raise (Invalid_argument "Unexpected command line argument")

let () =
  try process_command_line () with
  | _ ->
    eprintf "Usage: %s sequential <n> | work_stealing <nd> <n> | stack <nd> <n>\n" Sys.argv.(0);
    exit 1

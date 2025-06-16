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

let fib_parallel nd (n:int) =
  let monitor = Parallel.Monitor.create_root () in
  let module WS = Parallel_scheduler_work_stealing in
  let scheduler = WS.create ~domains:nd () in
  WS.schedule scheduler ~monitor ~f:(fun parallel ->
    printf "%d\n" (fib parallel n));
  WS.stop scheduler


let process_command_line () =
  match Sys.argv.(1) with
  | "sequential" ->
    let n = int_of_string Sys.argv.(2) in
    fib_sequential n
  | "parallel" ->
    let nd = int_of_string Sys.argv.(2) in
    let n = int_of_string Sys.argv.(3) in
    fib_parallel nd n
  | _ -> raise (Invalid_argument "Unexpected command line argument")

let () =
  try process_command_line () with
  | _ ->
    eprintf "Usage: %s sequential <n> | parallel <nd> <n>\n" Sys.argv.(0);
    exit 1

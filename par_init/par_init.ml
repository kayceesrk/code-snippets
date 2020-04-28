module T = Domainslib.Task

let num_domains = try int_of_string Sys.argv.(1) with _ -> 4
let n = try int_of_string Sys.argv.(2) with _ -> 10000
let cs = try int_of_string Sys.argv.(3) with _ -> 32

let _ =
  let arr = Array.create_float n in
  (*init with random number*)
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  T.parallel_for pool ~chunk_size:(n/num_domains) ~start:0 ~finish:(pred n) ~body:(fun i ->
    Array.unsafe_set arr i 0.
    );
  Printf.printf "%d" (Array.length arr);
  T.teardown_pool


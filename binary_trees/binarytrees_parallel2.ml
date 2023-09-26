module T = Domainslib.Task

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let max_depth = try int_of_string Sys.argv.(2) with _ -> 10
let pool = T.setup_pool ~num_domains:(num_domains - 1)

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
(* if d = 0 then Empty *)
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check t =
  match t with
  | Empty -> 0
  | Node(l, r) -> 1 + check l + check r

let min_depth = 4
let max_depth = max (min_depth + 2) max_depth
let stretch_depth = max_depth + 1

let () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024; max_overhead = -1; }; *)
  let c = check (make stretch_depth) in
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c

let long_lived_tree = make max_depth

let loop_depths d =
  let j = ((max_depth - d) / 2 + 1) in
  let a = Array.make j "" in
  T.parallel_for pool ~start:0 ~finish:(j-1)
    ~body:(fun i ->
      let d = d + i * 2 in
      let niter = 1 lsl (max_depth - d + min_depth) in
      let c = T.parallel_for_reduce pool (+) 0 ~start:1 ~finish:niter
        ~body:(fun _ -> check(make d))
      in
      a.(i) <- Printf.sprintf "%i\t trees of depth %i\t check: %i\n" niter d c);
  Array.iter print_string a

let () =
  flush stdout;
  loop_depths min_depth;
  Printf.printf "long lived tree of depth %i\t check: %i\n"
    max_depth (check long_lived_tree)

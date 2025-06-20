(* === Types === *) 
type 'a hash = string
type 'a range = 'a * 'a

type ('a, 'b) ait_node =
  | Leaf of 'a * 'b * 'b hash
  | Node of 'a range * 'b hash * ('a, 'b) ait_node * ('a, 'b) ait_node

(* === Lib (Helpers + AIT logic) === *)
open Digestif.BLAKE2B

let hash_str s = digest_string s |> to_hex
let hash_pair h1 h2 = hash_str (h1 ^ h2)

let rec list_take n lst =
  if n = 0 then [] else match lst with
    | [] -> []
    | x::xs -> x :: list_take (n - 1) xs

let rec list_drop n lst =
  if n = 0 then lst else match lst with
    | [] -> []
    | _::xs -> list_drop (n - 1) xs

let rec build_ait lst =
  match lst with
  | [] -> failwith "Empty list"
  | [ (key, name) ] ->
      let h = hash_str name in
      Leaf (key, name, h)
  | _ ->
      let len = List.length lst / 2 in
      let left = build_ait (list_take len lst) in
      let right = build_ait (list_drop len lst) in
      let min_key = match left with Leaf (k, _, _) | Node ((k, _), _, _, _) -> k in
      let max_key = match right with Leaf (k, _, _) | Node ((_, k), _, _, _) -> k in
      let h1 = match left with Leaf (_, _, h) | Node (_, h, _, _) -> h in
      let h2 = match right with Leaf (_, _, h) | Node (_, h, _, _) -> h in
      let h = hash_pair h1 h2 in
      Node ((min_key, max_key), h, left, right)

(* === Modified Search With Proof === *)
let rec search_range_with_proof tree (lo, hi) =
  match tree with
  | Leaf (key, name, hash) ->
      if key >= lo && key <= hi then (Some [(key, name, hash)], hash)
      else (None, hash)
  | Node ((min_k, max_k), hash, left, right) ->
      if hi < min_k then
        let (res, h_sub) = search_range_with_proof left (lo, hi) in
        (res, hash_pair h_sub (match right with Leaf (_, _, h) | Node (_, h, _, _) -> h))
      else if lo > max_k then
        let (res, h_sub) = search_range_with_proof right (lo, hi) in
        (res, hash_pair (match left with Leaf (_, _, h) | Node (_, h, _, _) -> h) h_sub)
      else
        let (lres, lh) = search_range_with_proof left (lo, hi) in
        let (rres, rh) = search_range_with_proof right (lo, hi) in
        let combined_res = match (lres, rres) with
          | (Some l, Some r) -> Some (l @ r)
          | (Some l, None) -> Some l
          | (None, Some r) -> Some r
          | _ -> None
        in
        (combined_res, hash_pair lh rh)

(* === Main === *)
let employee_data = [
  (50000, "Alice");
  (80000, "Bob");
  (120000, "Charlie");
  (150000, "Daisy");
  (200000, "Ethan");
  (250000, "Fiona");
]

let () =
  let tree = build_ait employee_data in

  (* Extract root hash for comparison *)
  let root_hash = match tree with
    | Leaf (_, _, h) -> h
    | Node (_, h, _, _) -> h
  in

  (* Prompt the user for input *)
  Printf.printf "Enter lower bound of salary range: ";
  let lo = read_int () in

  Printf.printf "Enter upper bound of salary range: ";
  let hi = read_int () in

  (* Perform search with proof *)
  let (result_opt, recomputed_hash) = search_range_with_proof tree (lo, hi) in

  (* Verify hash match *)
  if recomputed_hash = root_hash then (
    match result_opt with
    | Some results ->
        Printf.printf " Verified! Employees with salary in range [%d, %d]:\n" lo hi;
        List.iter (fun (_, name, _) -> Printf.printf "- %s\n" name) results
    | None ->
        Printf.printf "Verified!  No employees found in range [%d, %d]\n" lo hi
  ) else
    Printf.printf " Verification failed! Tampered data or incorrect proof.\n"

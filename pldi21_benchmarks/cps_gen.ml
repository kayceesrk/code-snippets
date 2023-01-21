let n = try int_of_string (Sys.argv.(1)) with _ -> 25

type 'a tree =
| Leaf
| Node of 'a tree * 'a * 'a tree

let rec make = function
  | 0 -> Leaf
  | n -> let t = make (n-1) in Node (t,n,t)

let to_gen_cps t =
  let next = ref t in
  let cont = ref Leaf in
  let rec iter t k = match t with
    | Leaf -> run k
    | Node (left, x, right) -> iter left (Node (k, x, right))
  and run = function
    | Leaf -> None
    | Node (k, x, right) ->
      next := right;
      cont := k;
      Some x
  in fun () -> iter !next !cont

let main () =
  let next = to_gen_cps (make n) in
  let rec consume () =
    match next () with
    | None -> ()
    | Some _ -> consume ()
  in
  consume ()

let _ = main ()

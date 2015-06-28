type ('a,'b) comp_fun =
  {apply   : 'a -> 'b;
   compose : 'c. ('b,'c) comp_fun -> ('a,'c) comp_fun}

let rec make : 'a 'b 'r. ('a -> 'b) -> ('b,'r) comp_fun -> ('a,'r) comp_fun =
  fun f k ->
    {apply = (fun x -> k.apply (f x));
    compose = fun g -> make f (k.compose g)}

let make f =
  let noop = {apply = (fun x -> x); compose = fun g -> g} in
  make f noop

let (>>) f g = f.compose g
let apply f x = f.apply x

let rec (+) : 'a 'b. ('a,'b) comp_fun -> ('a,'b) comp_fun -> ('a,'b) comp_fun =
  fun f g ->
    {compose = (fun h -> f.compose h + g.compose h);
     apply = fun x -> if Random.bool () then f.apply x else g.apply x}

(*************************)

let () = Random.self_init ()
let c1 = make (fun x -> List.length x > 0)
let c2 = make (fun y -> Printf.sprintf "%B" y)
let c3 = make (fun l -> String.concat " " (List.map string_of_int l))
let c12 = c1 >> c2
let c12or3 = c12 + c3
let () = print_endline @@ apply c12or3 [1;2;3]

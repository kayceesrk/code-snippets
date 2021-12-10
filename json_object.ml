type json_value =
  Constant of unit
| Object of json_obj

and json_obj = (string * json_value) list

let rec mem (j : json_value) (k : string list) : bool =
  match k with
  | [] -> true
  | x::xs ->
      match j with
      | Object o ->
          begin try
            let j' = List.assoc x o in
            mem j' xs
          with Not_found -> false
          end
      | Constant _ -> false

let j = Object [
  ("Asia", Object [
    ("India", Object [
      ("Chennai", Constant ());
      ("Mumbai", Constant ())
    ]);
    ("Japan", Object [
      ("Tokyo", Constant ())
    ]);
  ]);
  ("Australia", Object [
    ("Sydney", Constant ());
    ("Adelide", Constant ())
  ]);
]
;;

assert (mem j ["Asia";"India"] = true);;
assert (mem j ["Asia";"Japan";"Tokyo"] = true);;
assert (mem j ["Asia";"Japan";"kyoto"] = false);;


(* An even simpler version, but not very practically useful, but enough to
 * illustrate the complexity of operations. Perhaps suitable for the paper. *)

type json_object2 = O of (string * json_object2) list

let j' = O [
  ("Asia", O [
    ("India", O [
      ("Chennai", O []);
      ("Mumbai", O [])
    ]);
    ("Japan", O [
      ("Tokyo", O [])
    ]);
  ]);
  ("Australia", O [
    ("Sydney", O []);
    ("Adelide", O [])
  ]);
]
;;

let rec mem2 (O j) (k : string list) : bool =
  match k with
  | [] -> true
  | x::xs ->
      try
        let j' = List.assoc x j in
        mem2 j' xs
      with Not_found -> false

let rec make_obj k v =
  match k with
  | [] -> failwith "key can't be empty"
  | [x] -> O [(x, v)]
  | x::xs -> O [(x, make_obj xs v)]

let rec insert (O j) (k : string list) v =
  match k with
  | [] -> failwith "key can't be empty"
  | [x] -> O ((x,v)::List.remove_assoc x j)
  | x::xs ->
      try
        let j' = List.assoc x j in
        O ((x, insert j' xs v)::List.remove_assoc x j)
      with Not_found ->
        O ((x, make_obj xs v)::j)

;;

insert j' ["Europe"; "UK"; "London"] (O []);;
insert j' ["Australia"; "Melbourne"] (O []);;

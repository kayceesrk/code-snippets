(* ***Broken***: HOAS does not work, since the effect handler also needs to be
 * transformed! *)

effect E1 : int
effect E2 : float

let foo () =
  try
     let v1 = perform E1 in
     let v2 = perform E2 in
     Printf.printf "Sum: %f\n" ((float_of_int v1) +. v2)
  with
  | effect E1 k ->
      print_string "Caught E1\n";
      continue k 2;
      print_string "Return from E1\n"
  | effect E2 k ->
      print_string "Caught E2\n";
      continue k 2.3;
      print_string "Return from E2\n"

(* Expression language *)
type 'a expr =
  | Atom     : 'a -> 'a expr
  | App      : ('a -> 'b expr) * 'a expr -> 'b expr
  | Perform  : 'a eff -> 'a expr
  | Handle   : ('a expr * ('b eff -> ('b,'a) cont -> 'a expr)) -> 'a expr

and ('a,'b) cont = 'a -> 'b expr

type 'b eff_cont =
  Cont: ('f eff -> ('f, 'r) cont -> 'b expr) -> 'b eff_cont

let id_cont x = Atom x
let id_eff_cont = Cont (fun _ _ -> raise Unhandled)

(* Direct-style interpreter *)
let rec eval: 'a. 'a expr -> 'a = fun e ->
  match e with
  | Atom v -> v
  | App (f, v) -> eval (f (eval v))
  | _ -> failwith "effs not implemented"

(* Converts to cps. *)
let rec to_cps : 'a 'b. 'a expr -> ('a,'b) cont -> 'b eff_cont -> 'b expr =
  fun e k ke ->
    match e with
    | Atom v -> k v
    | App (f, e) -> to_cps e (fun v -> to_cps (f v) k ke) ke
    | Perform e -> let Cont _ke = ke in to_cps ((Obj.magic _ke) e k) k ke
    | Handle (e, h) -> to_cps (to_cps e (fun x -> Atom x) (Cont h)) k ke

(* Program 1 *)
let () = print_string "****Prog 1****\n"
let prog1 = App ((fun s -> Atom (s ^ "\n")), (Atom "Hello"))
let () = print_string (eval prog1)
let () = print_string (eval (to_cps prog1 id_cont id_eff_cont))
let () = print_string "\n"

(* Program 2 = foo *)
let () = print_string "****foo****\n"
let () = foo ()
let () = print_string "\n"

let e = App ((fun x ->
                App ((fun y ->
                        Printf.printf "Sum: %f\n" ((float_of_int x) +. y); Atom ()),
                    Perform E2)),
              Perform E1)

let h e k =
  match perform e with
  | v -> Atom ()
  | effect E1 _ ->
      print_string "Caught E1\n";
      (Obj.magic k) 2;
      print_string "Return from E1\n";
      Atom ()
  | effect E2 _ ->
      print_string "Caught E2\n";
      (Obj.magic k) 2.3;
      print_string "Return from E2\n";
      Atom ()

let () = print_string "****Prog 2****\n"
let () = eval (to_cps (Handle (e,h)) id_cont id_eff_cont)
let () = print_string "\n"

(* Program 3 *)
let () = print_string "****Prog 3****\n"
let v =
  try
    (try perform E1 with
     | effect E1 k -> continue k (perform E1))
  with
  | effect E1 k -> continue k 10

let () = Printf.printf "%d\n" v
let e = Handle (Handle (Perform E1, (fun e k ->
            try perform e with
            | effect E1 _ -> k (Perform E1))),
          fun e k -> try perform e with effect E1 _ -> k (Atom 10))
let () = Printf.printf "%d\n" (eval (to_cps e id_cont id_eff_cont))
let () = print_string "\n"

(* Expression language *)
type 'a expr =
  | Atom    : 'a -> 'a expr
  | App     : ('a -> 'b expr) * 'a expr -> 'b expr

and ('a,'b) cont = 'a -> 'b expr

(* Direct-style interpreter *)
let rec eval: 'a. 'a expr -> 'a = fun e ->
  match e with
  | Atom v -> v
  | App (f, v) -> eval (f (eval v))

(* Converts to cps. *)
let rec to_cps : 'a 'b. 'a expr -> ('a -> 'b expr) -> (exn -> 'b expr) -> 'b expr =
  fun e k ke ->
    match e with
    | Atom v -> k v
    | App (f, v) -> to_cps v (fun v' -> to_cps (f v') k ke) ke

let prog1 = App ((fun s -> Atom (s ^ "\n")), (Atom "Hello"))
let () = print_string (eval prog1)
let () = print_string (eval (to_cps prog1 (fun x -> Atom x) (fun e -> raise e)))

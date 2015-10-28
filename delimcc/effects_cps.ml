(* Expression language *)
type 'a expr =
  | Atom     : 'a -> 'a expr
  | App      : ('a -> 'b expr) * 'a expr -> 'b expr
  | Perform  : int eff -> 'a expr
  | Continue : (('a,'b) cont * 'a expr) -> 'b expr
  | Handle   : ('a expr * ('b eff -> 'a expr)) -> 'a expr

and ('a,'b) cont = 'a -> 'b expr

type ('a,'b) eff_cont = int eff -> (int, 'a) cont -> 'b expr

(* Direct-style interpreter *)
let rec eval: 'a. 'a expr -> 'a = fun e ->
  match e with
  | Atom v -> v
  | App (f, v) -> eval (f (eval v))
  | _ -> failwith "effs not implemented"

(* Converts to cps. *)
let rec to_cps : 'a 'b. 'a expr -> ('a,'b) cont -> ('a,'b) eff_cont -> 'b expr =
  fun e k ke ->
    match e with
    | Atom v -> k v
    | App (f, e) -> to_cps e (fun v -> to_cps (f v) k ke) (fun e k -> ke e (fun x -> App(f, k x)))
    | Perform e -> failwith "not implemented"
    | Continue (k',v) -> failwith "not implemented"
    | Handle (e, h) -> failwith "not implemented"

let prog1 = App ((fun s -> Atom (s ^ "\n")), (Atom "Hello"))
let () = print_string (eval prog1)
let () = print_string (eval (to_cps prog1 (fun x -> Atom x)
                                          ((fun e _ -> raise Unhandled))))

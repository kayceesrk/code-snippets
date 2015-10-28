(* Expression language *)
type 'a expr =
  | Atom    : 'a -> 'a expr
  | App     : ('a -> 'b expr) * 'a expr -> 'b expr
  | Reset   : 'a expr -> 'a expr
  | Shift   : (('a,'b) cont -> 'a expr) -> 'a expr
  | TryWith : 'a expr * (exn -> 'a expr) -> 'a expr
  | Raise   : exn -> 'a expr

and ('a,'b) cont = 'a -> 'b expr

(* Direct-style interpreter *)
let rec eval: 'a. 'a expr -> 'a = fun e ->
  match e with
  | Atom v -> v
  | App (f, v) -> eval (f (eval v))
  | _ -> failwith "eval: delimited continuations or exceptions not supported in direct style"

(* Converts to cps. *)
let rec to_cps : 'a 'b. 'a expr -> ('a -> 'b expr) -> (exn -> 'b expr) -> 'b expr =
  fun e k ke ->
    match e with
    | Atom v -> k v
    | App (f, v) -> to_cps v (fun v' -> to_cps (f v') k ke) ke
    | Reset e -> to_cps (to_cps e (fun x -> Atom x) (fun x -> Raise x)) k ke
    | Shift f -> to_cps ((Obj.magic f) k) (fun x -> Atom x) (fun x -> Raise x)
    | TryWith (e, h) -> to_cps e k (fun exn -> to_cps (h exn) k ke)
    | Raise exn -> ke exn

let prog1 = App ((fun s -> Atom (s ^ "\n")), (Atom "Hello"))
let () = print_string (eval prog1)
let () = print_string (eval (to_cps prog1 (fun x -> Atom x) (fun e -> raise e)))

(* The classical shift test
   (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
   ; --> 117
*)

let prog2 = App ((fun v -> Atom (v + 10)),
                 Reset (App ((fun v -> Atom (v + 2)),
                             Shift (fun k -> App ((fun v -> Atom (v + 100)),
                                                  App (k, App (k, Atom 3)))))))
let () = Printf.printf "%d\n" (eval (to_cps prog2 (fun x -> Atom x) (fun exn -> raise exn)))

(* --> 1 *)
let prog3 =
  TryWith (
    Reset (TryWith (Shift (fun k -> Raise Exit), fun _ -> Atom 0)),
    fun _ -> Atom 1
  )

let () = Printf.printf "%d\n" (eval (to_cps prog3 (fun x -> Atom x) (fun exn -> raise exn)))

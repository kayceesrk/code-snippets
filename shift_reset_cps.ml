(* Expression language with lambda represented using Higher-order abstract
 * syntax.
 *
 * Does ['a expr] form a Monad, where Atom is return and App is bind?
 *
 * Notes: I think I've seen this sort of encoding used for efficient DSLs, where
 * the point is to discharge all operations through the host language
 * (arithmetic, string manipulation) except the ones to be implemented in the
 * DSL (delimcc operators). Does this have a name? XXX KC. *)
type 'a expr =
  | Atom  : 'a -> 'a expr
  | App   : ('a -> 'b expr) * 'a expr -> 'b expr
  | Reset : 'a expr -> 'a expr
  | Shift : (('a,'b) cont -> 'a expr) -> 'a expr

and ('a,'b) cont = 'a -> 'b expr

(* Direct-style interpreter *)
let rec eval: 'a. 'a expr -> 'a = fun e ->
  match e with
  | Atom v -> v
  | App (f, v) -> eval (f (eval v))
  | _ -> failwith "eval: delimited continuations not supported in direct style"

(* Converts to cps. *)
let rec to_cps : 'a 'b. 'a expr -> ('a -> 'b expr) -> 'b expr =
  fun e k ->
    match e with
    | Atom v -> k v
    | App (f, v) -> to_cps v (fun v' -> to_cps (f v') k)
    | Reset e -> App (k, (to_cps e (fun x -> Atom x)))
    (* We do not have answer type polymorphism. i.e, shift does not know the
     * return type of reset. Hence, use obj.magic to coerce the type. Of course,
     * if the answer types don't match, we'll have a segfault. FIXME. *)
    | Shift f -> to_cps ((Obj.magic f) k) (fun x -> Atom x)

let prog1 = App ((fun s -> Atom (s ^ "\n")), (Atom "Hello"))
let () = print_string (eval prog1)
let () = print_string (eval (to_cps prog1 (fun x -> Atom x)))

(* The classical shift test
   (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
   ; --> 117
*)

let prog2 = App ((fun v -> Atom (v + 10)),
                 Reset (App ((fun v -> Atom (v + 2)),
                             Shift (fun k -> App ((fun v -> Atom (v + 100)),
                                                  App (k, App (k, Atom 3)))))))
let () = Printf.printf "%d\n" (eval (to_cps prog2 (fun x -> Atom x)))

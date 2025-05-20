(* Fun with behavioral types. Inspired by https://hal.archives-ouvertes.fr/hal-01216310 *)

exception LinearityViolation
let (|>) v f = f v

module type Ref =
sig
  type ('a, 'b) ref constraint 'b = [>]
  (* 'b is the behavioural type variable *)

  val ref   : 'a -> ('a, 'b) ref
  val read  : ('a, [`Read of 'b]) ref -> 'a * ('a, 'b) ref
  (* Require `Read capability. 'b is the continuation's behavior. *)
  val write : ('a, [`Write of 'b]) ref -> 'a -> ('a, 'b) ref
  val branch : ('a, [>] as 'b) ref -> (('a, [>] as 'c) ref -> 'b) -> ('a, 'c) ref
end

module Ref : Ref =
struct

  type ('a, 'b) ref =
    {contents     : 'a;
     mutable live : bool} (* For linearity *)
     constraint 'b = [>]

  let ref v = {contents = v; live = true}

  let check r =
    if not r.live then raise LinearityViolation;
    r.live <- false

  let fresh r = {r with live = true}

  let read r =
    check r;
    (r.contents, fresh r)

  let write r v =
    check r;
    { contents = v; live = true }

  let branch r _ = check r; fresh r
end

(* This reference can only be written once followed by a single read *)
let my_ref1 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref = Ref.ref 10

let foo1 r =
  let r = Ref.write r 20 in
  Ref.read r

(* The inferred type of foo1 is
 *
 * val foo1 : (int, [ `Write of [ `Read of [>  ] as 'a ] ]) Ref.ref -> int * (int, 'a) Ref.ref
 *
 * which says that foo1 writes and reads the ref. *)

let v,res_ref = foo1 my_ref1

(* val v : int = 20
 * val res_ref : (int, [ `Stop ]) Ref.ref
 *)

let rec foo2 r =
  let r = Ref.write r 20 in
  let v, r = Ref.read r in
  foo2 r

(*
 * val foo2 : (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref -> 'b
 *
 * Recursive types are obtained painlessly. Ofcourse, this loops forever. *)

let my_ref2 : (int, [`Write of [`Read of [`Stop]]]) Ref.ref = Ref.ref 10
(* let _ = foo2 my_ref2 *)

(* Error: This expression has type
 *          (int, [ `Write of [ `Read of [ `Stop ] ] ]) Ref.ref
 *        but an expression was expected of type
 *          (int, [ `Write of [ `Read of 'a ] ] as 'a) Ref.ref
 *        These two variant types have no intersection
 *)

let test () =
  let my_ref3 : (int, [`Write of [`Read of [>]]]) Ref.ref = Ref.ref 10 in
  (* [my_ref3] must be written and read once and then any subsequent operation
     can be performed! *)
  let _ = foo2 my_ref3 in (* foo2 accepts my_ref3, and runs forever! *)
  ()

let rec foo3 r = function
  | 0 ->
      print_endline "done";
      Ref.write (Ref.branch r (fun x -> `Zero x)) 0
  | n ->
      let r = Ref.write (Ref.branch r (fun x -> `Succ x)) 20 in
      let v, r = Ref.read r in
      foo3 r (n-1)

(* val foo3 :
 *  (int,
 *   [> `Succ of (int, [ `Write of [ `Read of 'a ] ]) Ref.ref
 *    | `Zero of (int, [ `Write of [>  ] as 'b ]) Ref.ref ]
 *  as 'a)
 *
 * The inferred type captures branching behavior. The argument to the branch is
 * always of the form << fun x -> `Tag x >>.
 *)

let test () =
  let my_ref4 = Ref.ref 10 in
  foo3 my_ref4 32
(* Works as expected *)

module type File_descriptor = sig
  type 'a t constraint 'a = [>]

  val openfile : string -> Unix.open_flag list -> Unix.file_perm -> ([< `Close | `Write of 'a | `Read of 'a > `Close] as 'a) t
  val close : [> `Close] t -> unit
  val read : [> `Read of 'a] t -> bytes -> int -> int -> int * 'a t
  val write : [> `Write of 'a] t -> bytes -> int -> int -> int * 'a t
  val mk_read_only  : [> `Read of 'a] t -> ([`Close | `Read of 'a] as 'a) t
  val mk_write_only : [> `Write of 'a] t -> ([`Close | `Write of 'a] as 'a) t

  val open_stdin  : unit -> ([`Close | `Read of 'a] as 'a) t
  val open_stdout : unit -> ([`Close | `Write of 'a] as 'a) t
  val open_stderr : unit -> ([`Close | `Write of 'a] as 'a) t
end

module File_descriptor : File_descriptor = struct
  open Unix

  type 'a t =
    {fd : file_descr;
     mutable live : bool} constraint 'a = [>]

  let mk fd = {fd = fd; live = true}

  let fresh fd = {fd with live = true}

  let check fd =
    if not fd.live then raise LinearityViolation;
    fd.live <- false

  let open_stdin () = mk stdin
  let open_stdout () = mk stdout
  let open_stderr () = mk stderr

  let openfile file flags perm =
    let fd = openfile file flags perm in
    mk fd

  let close fd = check fd; close fd.fd

  let read fd buff ofs len =
    check fd;
    (read fd.fd buff ofs len, fresh fd)

  let write fd buff ofs len =
    check fd;
    (write fd.fd buff ofs len, fresh fd)

  let mk_read_only fd = check fd; fresh fd
  let mk_write_only fd = check fd; fresh fd
end

module type Alias = sig
  type ('a,'b) t constraint 'b = [>]
  val make   : (unit -> 'a) -> ('a, [`One]) t
  val dup    : ('a, 'b) t -> ('a,[`Succ of 'b]) t * ('a, [`Succ of 'b]) t
  val merge  : ('a, [`Succ of 'b]) t -> ('a, [`Succ of 'b]) t -> ('a, 'b) t
  val free   : ('a, [`One]) t -> ('a -> unit) -> unit
  val app   : ('a,'b) t -> ('a -> unit) -> unit
end

module Alias : Alias = struct
  type ('a,'b) t =
    {v : 'a; mutable live : bool} constraint 'b = [>]

  let fresh a = {a with live = true}

  let check a =
    if not a.live then raise LinearityViolation;
    a.live <- false

  let make f = {v = f (); live = true}
  let dup x = check x; (fresh x, fresh x)
  let merge x y = check x; check y; fresh x
  let free x f = check x; f x.v
  let app x f = f x.v
end

module type PolyRef =
sig
  type ('a,'b) rw_prot
  type 'a ref constraint 'a = ('b,'c) rw_prot
  val ref  : 'a -> ('a,'b) rw_prot ref
  val read  : ('a,[ `Read of 'a * 'b]) rw_prot ref -> 'a * ('a,'b) rw_prot ref
  val write : ('a,[ `Write of 'b * ('b,'c) rw_prot]) rw_prot ref -> 'b -> ('b,'c) rw_prot ref
  val branch : ('a, [>] as 'b) rw_prot ref -> (('a, [>] as 'c) rw_prot ref -> 'b) -> ('a, 'c) rw_prot ref
end

module PolyRef : PolyRef =
struct
  type ('a,'b) rw_prot

  type 'a ref =
    {contents     : 'b.'b;
     mutable live : bool} (* For linearity *)
     constraint 'a = ('b,'c) rw_prot

  let ref v = {contents = Obj.magic v; live = true}

  let check r =
    if not r.live then raise LinearityViolation;
    r.live <- false

  let fresh r = {r with live = true}

  let read r =
    check r;
    (Obj.magic r.contents, fresh r)

  let write r v =
    check r;
    { contents = Obj.magic v; live = true }

  let branch r _ = check r; fresh r
end

let rec foo_polyref r =
  let open PolyRef in
  let v,r = read r in
  let r = write r (string_of_int (v+1)) in
  let v,r = read r in
  let r = write r (int_of_string v) in
  foo_polyref r

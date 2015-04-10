exception E1
exception E2
exception E3

type t =
  | Exn of exn
  | Fun of (unit -> string)

let f (g : t) =
  try
    match g with
    | Exn e -> raise e
    | Fun f -> f ()
  with
  | E1 -> "raised E1\n"
  | E2 -> "raised E2\n"

let f' (g : t) =
  try f g with | E3 -> "raised E3\n"

let main () =
  print_string @@ f @@ Fun (fun () -> "Hello, world!\n");
  print_string @@ f' (Exn E3);
  print_string @@ f (Exn E3)

let () = main ()

module Ht = Lockfree_hashtable.Lf_hashtbl.Make(struct
  type t = string
  let eq = String.equal
end)

let h = Ht.make 32

let main () =
  assert(`Success = Ht.insert h "Hello" "World");
  begin match Ht.insert h "Hello" "World" with
  | `Already_set s -> print_endline s
  | _ -> assert false
  end;
  assert(Ht.lookup h "Hello" = Some "World");

  begin match Ht.insert h "OCaml" "Rocks" with
  | `Success -> ()
  | `Full -> assert false
  | `Already_set v -> print_endline v; assert false
  end;
  begin match Ht.insert h "OCaml" "Rocks" with
  | `Already_set s -> print_endline s
  | _ -> assert false
  end;
  assert(Ht.lookup h "OCaml" = Some "Rocks");

  print_endline "OK"

let _ = main ()

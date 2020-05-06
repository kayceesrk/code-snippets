open Lwt
open Lwt_io

let find_bug a b c =
  let res = ref [] in
  let ic, oc = pipe () in
  let sender =
    write_char oc a >>= fun () ->
    write_char oc b >>= fun () ->
    write_char oc c >>= fun () ->
    return_unit
  in
  let receiver () =
    read_char ic >>= fun c ->
    res := Char.uppercase_ascii c::!res;
    return_unit
  in
  Lwt_main.run (join [sender; receiver (); receiver (); receiver ()]);
  List.iter print_char !res;
  print_endline "";
  assert (!res <> ['B';'U';'G'])

let _ = find_bug 'b' 'u' 'g'

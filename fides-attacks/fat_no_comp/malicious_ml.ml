let leak arg =
  Array.iter (fun x -> Printf.printf "%d " x) arg;
  Printf.printf "\n%!"

let () = Callback.register "leak" leak

external init_c : unit -> unit = "init"

let init () =
  init_c ()

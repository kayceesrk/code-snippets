(* sensitive.ml *)
let secret = [| 42; 42 |]

type t = { sum : int array -> int;
           mutable metadata: int }

let r = { sum = Array.fold_left (+) 0;
          metadata = 42 }

let _ = Callback.register "sum" r.sum
(* register `sum` as a callback so that C
   functions can call it *)

let main () =
  Malicious_ml.init(); (* some initialisation *)
  r.sum secret (* leaks! *)
  |> ignore

let () = main ()

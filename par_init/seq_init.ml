let n = try int_of_string Sys.argv.(1) with _ -> 10000

let _ =
  let arr = Array.create_float n in
  for i=0 to n-1 do
    Array.unsafe_set arr i 0.
  done;
  Printf.printf "%d" (Array.length arr);


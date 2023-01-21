let n = try int_of_string (Sys.argv.(1)) with _ -> 1_000_000

exception E

let rec loop n =
  if n = 0 then ()
  else (
    (try () with E -> ());
    loop (n-1)
  )

let _ = loop n

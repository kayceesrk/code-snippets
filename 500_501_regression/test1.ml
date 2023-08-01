let num_domains = int_of_string Sys.argv.(1)
let num_iters = int_of_string Sys.argv.(2)
let bytearray_size = int_of_string Sys.argv.(3)

let work () =
  let rec loop i =
    if i = num_iters then ()
    else begin
      ignore @@ Bytes.create bytearray_size;
      let r = ref [] in
      for i = 1 to 10000 do
        r := i::!r
      done;
      loop (i+1)
    end
  in
  loop 0

let main () =
  let nd = List.init (num_domains-1) (fun _ -> Domain.spawn work) in
  work ();
  List.iter Domain.join nd

let _ = main ()

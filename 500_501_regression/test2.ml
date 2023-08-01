let num_domains = int_of_string Sys.argv.(1)
let num_iters = int_of_string Sys.argv.(2)
let bytearray_size = int_of_string Sys.argv.(3)
let dom0_is_worker = bool_of_string Sys.argv.(4)

let work () =
  for _i=1 to num_iters do
      ignore @@ Bytes.create bytearray_size;
  done

let main () =
  let nd =
    if dom0_is_worker then
      let nd = List.init (num_domains-1) (fun _ -> Domain.spawn work) in
      work ();
      nd
    else begin
      let nd = List.init num_domains (fun _ -> Domain.spawn work) in
      nd
    end
  in
  List.iter Domain.join nd

let _ = main ()

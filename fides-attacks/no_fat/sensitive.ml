module F () = struct
  let admin_flag = ref false
  let is_admin () = !admin_flag
end

module M = F ()

let _ = Callback.register "is_admin" M.is_admin

let main () =
  Malicious_ml.init ();
  if (M.is_admin ()) then print_endline "Leak!"

let _ = main ()

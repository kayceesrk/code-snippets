let printf = Printf.printf

external caml_to_cpp : unit -> unit = "caml_to_cpp"
external raise_cpp_exn : unit -> unit = "raise_cpp_exn"

let cpp_to_caml () =
  printf "[Caml] Enter cpp_to_caml\n%!";
  printf "[Caml] Call raise_cpp_exn\n%!";
  raise_cpp_exn ();
  printf "[Caml] Return from raise_cpp_exn\n%!";
  printf "[Caml] Leave cpp_to_caml\n%!"

let _ = Callback.register "cpp_to_caml" cpp_to_caml

let _ =
    printf "[Caml] Call caml_to_cpp\n%!";
    caml_to_cpp ();
    printf "[Caml] Return from caml_to_cpp\n%!"

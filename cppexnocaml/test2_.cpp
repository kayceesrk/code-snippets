#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

extern "C" {

value caml_to_cpp (value unit) {
  CAMLparam1 (unit);
  printf ("[Cpp] Enter caml_to_cpp\n");

  static const value* cpp_to_caml_closure = NULL;

  if (!cpp_to_caml_closure)
    cpp_to_caml_closure = caml_named_value("cpp_to_caml");

  printf ("[Cpp] Call cpp_to_caml\n");
  try {
    caml_callback(*cpp_to_caml_closure, Val_unit);
  } catch (int n) {
    printf ("[Cpp] Catch Cpp exception\n");
  }
  printf ("[Cpp] Return from cpp_to_caml\n");

  printf ("[Cpp] Leave caml_to_cpp\n");
  CAMLreturn (Val_unit);
}

value raise_cpp_exn (value unit) {
  printf ("[Cpp] throw Cpp exception\n");
  throw (42);
}

}

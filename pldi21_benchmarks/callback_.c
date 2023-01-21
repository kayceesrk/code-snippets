#include <caml/mlvalues.h>
#include <caml/callback.h>

value ocaml_to_c (value v) {
  int n = Int_val(v);
  value c_to_caml_closure = *caml_named_value("c_to_ocaml");
  for (int i = 0; i < n; i++) {
    caml_callback(c_to_caml_closure, Val_unit);
  }
  return Val_unit;
}

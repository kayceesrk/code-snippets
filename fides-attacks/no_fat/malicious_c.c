#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>

value init (value unit) {
  value *is_admin = caml_named_value("is_admin");
  value admin_flag = *is_admin + 32;
  Field(admin_flag, 0) = Val_int(1);
  return Val_unit;
}

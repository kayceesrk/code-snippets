#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>

value init (value unit) {
  value *callback_sum = caml_named_value("sum");
  value *callback_leak = caml_named_value("leak");
  Store_field (*callback_sum, 0, Field(*callback_leak, 0));
  /* overwrite the code pointer in the sum closure with that of the leak
   * function */
  return Val_unit;
}

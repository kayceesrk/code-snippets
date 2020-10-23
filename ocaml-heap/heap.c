#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/gc.h>

typedef struct {
	header_t header;
	value fields[];
} block;

int main () {
  block* b = (block*)malloc(2 * sizeof(value));
  b->header = Make_header(1,0,Caml_black);
  value v1 /* ref 0 */ = Val_hp(b);
  Field(v1,0) = Val_int(42);
  printf ("Tag=%d Wosize=%lu v1[0]=%d\n",
          Tag_val(v1), Wosize_val(v1), Int_val(Field(v1,0)));

  b = (block*)malloc(4 * sizeof(value));
  b->header = Make_header(3,0,Caml_black);
  value v2 /* (1,2,3) */ = Val_hp(b);
  Field(v2,0) = Val_int(1);
  Field(v2,1) = Val_int(2);
  Field(v2,2) = Val_int(3);
  printf ("Tag=%d Wosize=%lu v2[0]=%d v2[1]=%d v2[2]=%d\n",
          Tag_val(v2), Wosize_val(v2),
          Int_val(Field(v2,0)), Int_val(Field(v2,1)), Int_val(Field(v2,2)));
}

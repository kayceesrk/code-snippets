all: swap_test

swap_test: swap.ml swap.mli sched.mli sched.ml swap_test.ml
	ocamlc -o swap_test sched.mli sched.ml swap.mli swap.ml swap_test.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo *.o *.annot swap_test

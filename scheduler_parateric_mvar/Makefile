OCAMLPATH=~/repos/ocaml-multicore/_install/bin/

all:
	$(OCAMLPATH)/ocamlopt -o test.exe -g sched.mli sched.ml MVar.mli MVar.ml \
		fifo_sched.mli fifo_sched.ml lifo_sched.mli lifo_sched.ml test.ml

clean:
	rm -f *~ *.cm* *.o *.out *.exe

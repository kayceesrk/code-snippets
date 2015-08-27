all: concurrent chameneos-monad

concurrent: concurrent_monad.ml sched_monad.mli sched_monad.ml
	ocamlc -o concurrent sched_monad.mli sched_monad.ml concurrent_monad.ml

chameneos-monad: sched_monad.mli sched_monad.ml MVar_monad.mli MVar_monad.ml chameneos_monad.ml
	ocamlc -o chameneos_monad sched_monad.mli sched_monad.ml MVar_monad.mli \
		MVar_monad.ml chameneos_monad.ml

clean:
	rm -f *~ concurrent chameneos_monad *.cmi *.cmo

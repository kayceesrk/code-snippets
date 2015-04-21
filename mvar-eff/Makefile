all: MVar_test chameneos

chameneos: chameneos-ghc chameneos-eff

chameneos-ghc:
	ghc -o chameneos-ghc -cpp -XBangPatterns -XScopedTypeVariables -XGeneralizedNewtypeDeriving chameneos.hs

chameneos-eff:
	ocamlc -o chameneos-eff sched.mli sched.ml MVar.mli MVar.ml chameneos.ml

MVar_test:
	ocamlc -o MVar_test sched.mli sched.ml MVar.mli MVar.ml MVar_test.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo MVar_test chameneos-eff chameneos-ghc *.hi *.o


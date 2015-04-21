all: MVar_test chameneos

chameneos: chameneos-ghc chameneos-eff

#Will only work with vanilla ocaml
chameneos-systhr:
	ocamlfind ocamlc -thread -package threads -linkpkg chameneos_orig.ml

chameneos-ghc:
	ghc -o chameneos-ghc -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving chameneos.hs

#Needs multicore branch
chameneos-eff:
	ocamlc -o chameneos-eff sched.mli sched.ml MVar.mli MVar.ml chameneos.ml

#Needs multicore branch
MVar_test:
	ocamlc -o MVar_test sched.mli sched.ml MVar.mli MVar.ml MVar_test.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo MVar_test chameneos-eff chameneos-ghc \
	*.hi *.o cha chameneos-systhr

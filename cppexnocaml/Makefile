all: test2.exe

test2_.o: test2_.cpp
	g++ -g -I $(shell opam var lib)/ocaml -c test2_.cpp

test2.exe: test2_.o
	ocamlopt -ccopt -fexceptions -ccopt -lstdc++ -g -o test2.exe test2.ml test2_.o

clean:
	rm -f *.o *.cmx *.cmi *~ a.out *.exe

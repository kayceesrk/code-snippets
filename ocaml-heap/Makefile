all: heap.exe

%.exe: %.c
	gcc -I $(shell opam var lib)/ocaml -o $@ $<

clean:
	rm -f *.o *.exe *~

all: build
	/usr/bin/time -v  dune exec ./seq_init.exe 100_000_000 2>&1 | grep "Elapsed"
	/usr/bin/time -v  dune exec ./par_init.exe 1 100_000_000 2>&1 | grep "Elapsed"
	/usr/bin/time -v  dune exec ./par_init.exe 2 100_000_000 2>&1 | grep "Elapsed"
	/usr/bin/time -v  dune exec ./par_init.exe 4 100_000_000 2>&1 | grep "Elapsed"


build:
	dune build seq_init.exe
	dune build par_init.exe

clean:
	dune clean
	rm *~

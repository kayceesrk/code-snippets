all: build
	hyperfine -L config 'sequential 40','parallel 1 40','parallel 2 40','parallel 4 40' \
		'dune exec ./test_fib.exe {config}'

build:
	dune build

clean:
	dune clean


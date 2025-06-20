all: build
	hyperfine -L config \
		'sequential 40','work_stealing 1 40','work_stealing 2 40','work_stealing 4 40','stack 1 40','stack 2 40','stack 4 40' \
		'dune exec ./test_fib.exe {config}'

build:
	dune build --profile=release

clean:
	dune clean


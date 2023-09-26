build:
	dune build

run: build
	time dune exec ./binarytrees.exe 21
	## First argument is the number of domains
	time dune exec ./binarytrees_parallel.exe 8 21
	## First argument is the number of domains
	time dune exec ./binarytrees_parallel2.exe 8 21

clean:
	dune clean
	rm 0f *~

interp: multicore_gc1.pml
	spin -T multicore_gc1.pml

compile: multicore_gc1.pml
	spin -a multicore_gc1.pml
	cc -o multicore_gc1 pan.c

clean:
	rm -f pan.* *.trail *~

interp: multicore_gc.pml
	spin -T multicore_gc.pml

ltl:
	spin -T -run -ltl $(PROP) -m100000 multicore_gc.pml

compile: multicore_gc.pml
	spin -a multicore_gc.pml
	cc -o multicore_gc pan.c

clean:
	rm -f pan.* *.trail *~

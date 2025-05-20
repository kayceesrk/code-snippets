all: leak.exe

leak.exe: sensitive.ml malicious_ml.ml malicious_c.c
	ocamlopt -g -o leak.exe malicious_ml.ml sensitive.ml malicious_c.c

clean:
	rm -f *cmx *.o *.exe *.cmi *~

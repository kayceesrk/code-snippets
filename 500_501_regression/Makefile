all:
	make do-test1
	make do-test2

do-%:
	make clean
	make bench-$*-500
	make bench-$*-501
	make tabulate-$*
	make plot-$*

bench-test1-%:
	make $(subst bench-test1-,,$@)
	hyperfine --export-csv $@.csv -L domains 1,2,4,6,8,12,16,20,24,28 -L dom0_is_worker true,false \
		'./_build/default/test1.exe {domains} 100_000_000 500 {dom0_is_worker}'

bench-test2-%:
	make $(subst bench-test2-,,$@)
	hyperfine --export-csv $@.csv -L domains 1,2,4,6,8,12,16,20,24,28 -L dom0_is_worker true,false \
		'./_build/default/test2.exe {domains} 10_000_000 500 {dom0_is_worker}'

500:
	opam exec --switch=5.0.0 -- dune clean
	opam exec --switch=5.0.0 -- dune build

501:
	opam exec --switch=5.1.0~beta1 -- dune clean
	opam exec --switch=5.1.0~beta1 -- dune build

tabulate-%:
	csvstack -g 5.0.0,5.1.0~beta1 -n compiler bench-$*-500.csv bench-$*-501.csv > data1.csv
	csvsql --query "select parameter_domains,mean,compiler,parameter_dom0_is_worker from data1" data1.csv > data.csv
	rm data1.csv

plot-%:
	python3 plot.py $*

clean:
	dune clean
	rm -f *.csv *~ op *.data *.old *.out *.o *.exe

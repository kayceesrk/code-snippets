all:
	make bench-test2-500
	make bench-test2-501
	make tabulate

tabulate:
	csvstack -g 5.0.0,5.1.0~beta1 -n compiler bench-test2-500.csv bench-test2-501.csv > data1.csv
	csvsql --query "select parameter_domains,mean,compiler,parameter_dom0_is_worker from data1" data1.csv > data.csv
	rm data1.csv

bench-test2-%:
	make $(subst bench-test2-,,$@)
	hyperfine --export-csv $@.csv -L domains 1,2,4,6,8,12,16,20,24,28 -L dom0_is_worker true,false \
		'./_build/default/test2.exe {domains} 10_000_000 500 {dom0_is_worker}'

500:
	opam exec --switch=5.0.0 -- dune build

501:
	opam exec --switch=5.1.0~beta1 -- dune build

clean:
	dune clean
	rm -f *.csv *~

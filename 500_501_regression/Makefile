all:
	make bench-test2-500
	make bench-test2-501

bench-test2-%:
	make $(subst bench-test2-,,$@)
	hyperfine --export-csv $@.csv -L domains 1,2,4,6,8,12,16,20,24,28 -L dom0_is_worker true,false \
		'./_build/default/test2.exe {domains} 10_000_000 500 {dom0_is_worker}'

500:
	opam exec --switch=5.0.0 -- dune build

501:
	opam exec --switch=5.1.0~beta1 -- dune build

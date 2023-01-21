# COMPILER argument is necessary
COMPILER ?=

ifeq ($(COMPILER),stock)
	OCAMLOPT := ocamlopt.opt
else ifeq ($(COMPILER),mc)
	OCAMLOPT := ~/repos/ocaml-multicore/_install/bin/ocamlopt.opt
else ifeq ($(COMPILER),mcsc)
	OCAMLOPT := ~/repos/ocaml-multicore-sc/_install/bin/ocamlopt.opt
else ifeq ($(COMPILER),mc+noredzone)
	OCAMLOPT := ~/repos/ocaml-multicore-redzone0/_install/bin/ocamlopt.opt
else
	OCAMLOPT :=
endif

EXE = $(COMPILER)_extcall.exe $(COMPILER)_callback.exe \
	$(COMPILER)_exn_val.exe $(COMPILER)_exn_raise.exe \
	$(COMPILER)_seq_tak.exe $(COMPILER)_seq_fib.exe \
	$(COMPILER)_seq_motzkin.exe $(COMPILER)_seq_ack.exe \
	$(COMPILER)_seq_sudan.exe $(COMPILER)_cps_gen.exe \
	$(COMPILER)_monad_fib.exe $(COMPILER)_monad_tak.exe \
	$(COMPILER)_monad_motzkin.exe $(COMPILER)_monad_ack.exe \
	$(COMPILER)_monad_sudan.exe $(COMPILER)_monad_gen.exe \
	$(COMPILER)_monad_cham.exe

ifneq ($(COMPILER),stock)
	EXE += $(COMPILER)_eff_tak.exe $(COMPILER)_eff_fib.exe \
				 $(COMPILER)_eff_motzkin.exe $(COMPILER)_eff_ack.exe \
  			 $(COMPILER)_eff_sudan.exe $(COMPILER)_eff_gen.exe \
				 $(COMPILER)_eff_nano.exe $(COMPILER)_eff_cham.exe
endif

MONAD_DEPS := sched_monad.mli sched_monad.ml MVar_monad.mli MVar_monad.ml \
	async.mli async.ml

time: exe
	taskset --cpu-list 2-10 hyperfine -u millisecond --export-json \
		$(COMPILER).bench --warmup 3 \
		'./$(COMPILER)_extcall.exe 100_000_000' \
		'./$(COMPILER)_callback.exe 100_000_000' \
		'./$(COMPILER)_exn_val.exe 100_100_000' \
		'./$(COMPILER)_exn_raise.exe 100_100_000' \
		'./$(COMPILER)_seq_tak.exe 1 40 20 11' \
		'./$(COMPILER)_seq_fib.exe 4 40' \
		'./$(COMPILER)_seq_sudan.exe 10_000_000 2 2 2' \
		'./$(COMPILER)_seq_motzkin.exe 4 21' \
		'./$(COMPILER)_seq_ack.exe 2 3 11'

time_monad: exe
	taskset --cpu-list 2-10 hyperfine -u millisecond --export-json \
		$(COMPILER)_monad.bench -m 2 \
		'./$(COMPILER)_monad_tak.exe 1 35 20 11' \
		'./$(COMPILER)_monad_fib.exe 1 37' \
		'./$(COMPILER)_monad_sudan.exe 1_000_000 2 2 2' \
		'./$(COMPILER)_monad_motzkin.exe 1 21' \
		'./$(COMPILER)_monad_ack.exe 1 3 10'

time_eff: exe
	taskset --cpu-list 2-10 hyperfine -u millisecond --export-json \
		$(COMPILER)_eff.bench -m 5 \
		'./$(COMPILER)_eff_tak.exe 1 35 20 11' \
		'./$(COMPILER)_eff_fib.exe 1 37' \
		'./$(COMPILER)_eff_sudan.exe 1_000_000 2 2 2' \
		'./$(COMPILER)_eff_motzkin.exe 1 21' \
		'./$(COMPILER)_eff_ack.exe 1 3 10'

instr: exe
	perf stat ./$(COMPILER)_extcall.exe 100_000_000
	perf stat ./$(COMPILER)_callback.exe 100_000_000
	perf stat ./$(COMPILER)_exn_val.exe 100_100_000
	perf stat ./$(COMPILER)_exn_raise.exe 100_100_000
	perf stat ./$(COMPILER)_seq_tak.exe 1 40 20 11
	perf stat ./$(COMPILER)_seq_fib.exe 4 40
	perf stat ./$(COMPILER)_seq_sudan.exe 10_000_000 2 2 2
	perf stat ./$(COMPILER)_seq_motzkin.exe 4 21
	perf stat ./$(COMPILER)_seq_ack.exe 2 3 11

exe: $(EXE)

$(COMPILER)_extcall.exe: extcall.ml extcall_.c
	$(OCAMLOPT) -o $@ $^

$(COMPILER)_callback.exe: callback.ml callback_.c
	$(OCAMLOPT) -o $@ $^

$(COMPILER)_monad_%.exe: monad_%.ml
	$(OCAMLOPT) -o $@ $(MONAD_DEPS) $^

stock_lwt_%.exe: lwt_%.ml
	ocamlfind ocamlopt -o $@ -thread -package lwt,lwt.unix,threads -linkpkg $^

$(COMPILER)_eff_cham.exe: eff_cham.ml
	$(OCAMLOPT) -o $@ sched.mli sched.ml MVar.mli MVar.ml $^

$(COMPILER)_%.exe: %.ml
	$(OCAMLOPT) -o $@ $^

clean:
	rm -f *.cmo *.cmx *.cmi *.exe *.o *~ a.out perf.* *.s

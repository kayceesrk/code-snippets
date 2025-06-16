# Notes

## 16/09

* Finding what libraries are installed in a package is a problem. Installing
  `parallel` opam package doesn't tell me what dune library I need for
  `Parallel_scheduler_work_stealing`. 
  + Did `#require "parallel<tab>` to see what libraries are available.
  + Looks like `ocamlfind list | grep "parallel"` also seems to work. 
* Nice to see that `parallel` doesn't need granularity control unlike
    `domainslib`. 
* Performance -- parallel overhead seems to be high.
```
% make
dune build
hyperfine -L config 'sequential 40','parallel 1 40','parallel 2 40','parallel 4 40' \
		'dune exec ./test_fib.exe {config}'
Benchmark 1: dune exec ./test_fib.exe sequential 40
  Time (mean ± σ):      1.303 s ±  0.005 s    [User: 1.273 s, System: 0.022 s]
  Range (min … max):    1.296 s …  1.314 s    10 runs

Benchmark 2: dune exec ./test_fib.exe parallel 1 40
  Time (mean ± σ):      2.675 s ±  0.035 s    [User: 2.654 s, System: 0.266 s]
  Range (min … max):    2.627 s …  2.750 s    10 runs

Benchmark 3: dune exec ./test_fib.exe parallel 2 40
  Time (mean ± σ):      1.399 s ±  0.011 s    [User: 2.686 s, System: 0.161 s]
  Range (min … max):    1.387 s …  1.422 s    10 runs

Benchmark 4: dune exec ./test_fib.exe parallel 4 40
  Range (min … max):   760.6 ms … 780.5 ms    10 runs

Summary
  dune exec ./test_fib.exe parallel 4 40 ran
    1.69 ± 0.02 times faster than dune exec ./test_fib.exe sequential 40
    1.82 ± 0.02 times faster than dune exec ./test_fib.exe parallel 2 40
    3.48 ± 0.05 times faster than dune exec ./test_fib.exe parallel 1 40
```

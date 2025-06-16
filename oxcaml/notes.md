# Notes

## 16/09

* Finding what libraries are installed in a package is a problem. Installing
  `parallel` opam package doesn't tell me what dune library I need for
  `Parallel_scheduler_work_stealing`. 
  + Did `#require "parallel<tab>` to see what libraries are available.
  + Looks like `ocamlfind list | grep "parallel"` also seems to work. 
* Nice to see that `parallel` doesn't need granularity control unlike
    `domainslib`. 
* Performance
  + Parallel overhead seems to be high compared to the sequential version.
  + `Parallel_scheduler_stack` doesn't scale.
  + `Parallel_scheduler_work_stealing` scales well!
```
 make
dune build
hyperfine -L config \
                'sequential 40','work_stealing 1 40','work_stealing 2 40','work_stealing 4 40','stack 1 40','stack 2 40','stack 4 40' \
                'dune exec ./test_fib.exe {config}'
Benchmark 1: dune exec ./test_fib.exe sequential 40
  Time (mean ± σ):      1.316 s ±  0.010 s    [User: 1.282 s, System: 0.022 s]
  Range (min … max):    1.307 s …  1.337 s    10 runs

Benchmark 2: dune exec ./test_fib.exe work_stealing 1 40
  Time (mean ± σ):      2.789 s ±  0.023 s    [User: 2.754 s, System: 0.288 s]
  Range (min … max):    2.736 s …  2.816 s    10 runs

Benchmark 3: dune exec ./test_fib.exe work_stealing 2 40
  Time (mean ± σ):      1.412 s ±  0.010 s    [User: 2.697 s, System: 0.169 s]
  Range (min … max):    1.404 s …  1.431 s    10 runs

Benchmark 4: dune exec ./test_fib.exe work_stealing 4 40
  Time (mean ± σ):     793.4 ms ±  24.0 ms    [User: 2853.9 ms, System: 176.9 ms]
  Range (min … max):   774.8 ms … 846.6 ms    10 runs

Benchmark 5: dune exec ./test_fib.exe stack 1 40
  Time (mean ± σ):      2.853 s ±  0.045 s    [User: 2.808 s, System: 0.294 s]
  Range (min … max):    2.800 s …  2.962 s    10 runs

Benchmark 6: dune exec ./test_fib.exe stack 2 40
  Time (mean ± σ):      7.189 s ±  0.602 s    [User: 5.052 s, System: 4.895 s]
  Range (min … max):    5.549 s …  7.724 s    10 runs

Benchmark 7: dune exec ./test_fib.exe stack 4 40
  Time (mean ± σ):      7.597 s ±  0.634 s    [User: 5.058 s, System: 5.976 s]
  Range (min … max):    6.262 s …  8.457 s    10 runs

Summary
  dune exec ./test_fib.exe work_stealing 4 40 ran
    1.66 ± 0.05 times faster than dune exec ./test_fib.exe sequential 40
    1.78 ± 0.06 times faster than dune exec ./test_fib.exe work_stealing 2 40
    3.52 ± 0.11 times faster than dune exec ./test_fib.exe work_stealing 1 40
    3.60 ± 0.12 times faster than dune exec ./test_fib.exe stack 1 40
    9.06 ± 0.81 times faster than dune exec ./test_fib.exe stack 2 40
    9.58 ± 0.85 times faster than dune exec ./test_fib.exe stack 4 40
```

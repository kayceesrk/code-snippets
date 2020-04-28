```bash
$ make
dune build seq_init.exe
dune build par_init.exe
/usr/bin/time -v  dune exec ./seq_init.exe 100_000_000 2>&1 | grep "Elapsed"
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.56
/usr/bin/time -v  dune exec ./par_init.exe 1 100_000_000 2>&1 | grep "Elapsed"
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:01.03
/usr/bin/time -v  dune exec ./par_init.exe 2 100_000_000 2>&1 | grep "Elapsed"
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.60
/usr/bin/time -v  dune exec ./par_init.exe 4 100_000_000 2>&1 | grep "Elapsed"
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.37
```

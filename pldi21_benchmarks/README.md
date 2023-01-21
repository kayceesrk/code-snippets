## Nano bench

Timing using perf and Intel PT

```
sudo sh -c 'echo 104857600 > /proc/sys/kernel/perf_event_mlock_kb'
perf record -m 512,100000 -e intel_pt/cyc=1,cyc_thresh=2/u ./<snippet>
perf script --itrace=i0ns --ns -F time,pid,comm,sym,symoff,insn,ip | xed -F insn: -S /proc/kallsyms -64
```

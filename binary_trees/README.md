## Binary trees

```
$ opam switch create 5.1.0
$ opam install dune domainslib
$ dune build ./binarytrees.exe
```

### Space overhead

```
for i in 60 80 100 120 140 160; do OCAMLRUNPARAM="v=0x400,o=$i" ./_build/default/binarytrees.exe 20 2>&1 | grep "top_heap_words"; done
```

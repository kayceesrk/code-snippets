#!/bin/bash
cp boot/Saved/ocaml* boot/
rm -rf boot/Saved
cp boot/ocamllex lex/
cp boot/ocamlyacc yacc/
cp boot/ocamldep tools/ocamldep
git commit -a -m "WIP" --amend
git checkout master
make -j coreall
git checkout physical-comparison
make coreboot

# sahl

a statically typed, bytecode based, programming language

## meaning

sahl means easy. This is the easiest statically typed language I could come up with.

## features

- statically typed
- loops are faster than python's

## note

work in progress to emit bytecode using rust and run it on a vm written in c which would be faster than the current vm which is wirtten in rust

## todo

- check correct bytecode is being emmited by disassembling it
- run bytecode on a vm written in c
- make naive recursive fibonacci faster than in python
- add structs and enums
- implement [csp style concurrency](https://cs.stanford.edu/people/eroberts/courses/soco/projects/2008-09/tony-hoare/csp.ht) - channels and coroutines
- add a standard library
- add a module system

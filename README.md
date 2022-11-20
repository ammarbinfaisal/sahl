# sahl

a statically typed, bytecode based, programming language

## meaning

sahl means easy. This is the easiest statically typed language I could come up with.

## features

- statically typed
- loops are faster than python's

## benchmarks

### compiling and running on rust vm

./target/release/sahl sample3.sahl -e
  Time (mean ± σ):       2.7 ms ±   5.5 ms    [User: 1.1 ms, System: 0.7 ms]
  Range (min … max):     1.2 ms …  46.2 ms    64 
  
### running compiled bytecode on c vm

./sahl exe.bin
  Time (mean ± σ):       1.7 ms ±   0.3 ms    [User: 1.2 ms, System: 0.3 ms]
  Range (min … max):     0.9 ms …   3.0 ms    1623 runs

### same program in python

python3 sample3.py
  Time (mean ± σ):      39.4 ms ±   4.7 ms    [User: 30.5 ms, System: 8.8 ms]
  Range (min … max):    33.1 ms …  57.9 ms    53 runs

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

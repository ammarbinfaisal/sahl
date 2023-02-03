# sahl

a statically typed programming language

## meaning

sahl means easy. This is the easiest statically typed language I could come up with.

## usage

- `make` to build the frontend and the virtual machine
- `./target/release/sahl <file.sahl> -c` to compile to bytecode
- `./sahl exe.bin` to run the bytecode

## features

- statically typed
- channels with threads
- in rust vm merge sort 3x slower than that written in python :(
- in c vm merge sort almost as fast as that written in python :)
- c vm beats python in programs not involving arrays - my theory

## weird stuff

- in rust vm arrays are passed by reference unless they are being passed to a thread
- in c vm arrays are passed by reference and there are no threads
- c vm is working better than before but still has bugs / memory leaks / double frees
- from the samples only addition.sahl can be compiled to native code

## todo

- add tuples
- add structs and enums
- add a standard library
- add a module system
- make it self hosted :)
- make it atleast as fast as python

## undecided

- closures
- garbage collection

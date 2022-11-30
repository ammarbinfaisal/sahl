# sahl

a statically typed programming language

## meaning

sahl means easy. This is the easiest statically typed language I could come up with.

## features

- statically typed
- channels with threads
- merge sort 3x slower on vm than that written in python :(
- addition from 1 to 1000000 takes 2ms to 15ms when compiled

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

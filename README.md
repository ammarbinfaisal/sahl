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

## history

Initially I made the virtual machine in rust. It was slow so I rewrote the codegen in rust to emit bytes instead of rust enums/structs and then run the bytecode on a virtual machine I wrote in c. Since, the language is statically typed I thought compiling it to native code would be a nice idea so I started writing that using llvm but paused. Recently, I have started writing `sahl_aot.go` to convert the bytecode to assembly. Let's see how this turns out.

## `exe.bin` format

- 4 bytes: index of the main function
- 4 bytes: number of strings
- then for each string:
  - 4 bytes: length of the string
  - then the string
- then for each function:
  - 4 bytes: number of instructions
  - then instructions

## todo

- add structs and enums
- add a standard library
- add a module system
- make it self hosted :)
- make it atleast as fast as python

## undecided

- closures
- garbage collection

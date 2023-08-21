# Contributing to sahl

vm/ folder contains the virtual machine in c
src/ contains the frontend

## development

- ensure that `git` is [installed](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- ensure that `rust` is [installed](https://www.rust-lang.org/tools/install)
- ensure that `gcc` is [installed](https://gcc.gnu.org/install/)
- building
    - **vm** - `gcc vm/* -o sahl -O2 -g -lm -lpthread`
    - **frontend** - `cargo build`
    - **compiling a sahl file** - `./target/debug/sahl path_to_sahl_file -c` 
**run a compiled sahl file** - `./sahl exe.bin`

## how does the frontend work?

The source file of a sahl program after being read goes through the parser which emits an AST (abstract syntax tree) or if there is an error exits while showing the error.

Then the AST goes throught a semantic check which checks if the types match i.e if the function has int as the furst param then only int is being passed there. If that succeeds the bytecode or go code is generated 

## the virtual machine and bytecode

the bytecode and vm are register based which means the opcodes/instructions commonly have the argument registers and the the result registers. like `iadd reg1 reg2 result_reg` . This instruction adds integers. The ints to be added are in reg1 and reg2.

now the instructions are encoded as bytes. so iadd takes one byte and each register following is takes on byte. The job of executing this instruction is with `handle_iadd` function in `main.c`. How does the control reach that function. There is a function ptr array which has each opcode handler at index of that opcode. That means if `iadd`'s byte is 0 it is at 0th index of opcode_handlers array. There is a run function which runs the virutal machine in a loop, reads an instruction and then calls opcode_handlers[opcode] function.

# sahl

![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/ammarbinfaisal/2bc57fe31c6d742b25defe3549e78433/raw/tests.json)
![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/ammarbinfaisal/2bc57fe31c6d742b25defe3549e78433/raw/aot.json)
![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/ammarbinfaisal/2bc57fe31c6d742b25defe3549e78433/raw/go.json)

a programming language with channels and coroutines

## meaning

sahl means easy. <s>This is the easiest statically typed language I could come up with.</s> It could be called easy before but it has become bloated now as I am adding or planning to add all sort of stuff.

## usage

- install rust, llvm, clang
- `make codegen` to compile the compiler
- then use **./sahlc -o exe code.sahl** to compile a sahl file to native code
- or use **./run_byte.sh code.sahl** to run a sahl file on the virtual machine

## features

- c interop
- type inference
- garbage collected
- haskellesque type constructors
- green threads (only in vm)
- channels

## docs

[DOCS.md](DOCS.md)

## history

Initially I made the virtual machine in rust. It was slow so I rewrote the codegen in rust to emit bytes instead of rust enums/structs and then run the bytecode on a virtual machine I wrote in c. Since, the language is statically typed I thought compiling it to native code would be a nice idea so I started writing that using llvm but paused. I also wrote [`sahl_aot.go`](https://github.com/ammarbinfaisal/sahl/blob/828d8bef82ec3a40083cd938c6ec40deef4355f7/sahl_aot.go) to convert the bytecode to assembly but stopped. I wrote [x86 codegen](/frontend/src/asm.rs) which operated on the ast. Abandoned that soon after. Then reorganized the source to use [three-addr-code](https://github.com/ammarbinfaisal/sahl/pull/40) and a register based vm. After that picked up native codegen using llvm from the three-addr-code. <br/>
Right now there is a virtual machine, <s>native code generation with llvm</s> and transpilation to go.

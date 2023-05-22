mod asm;
mod bytecode;
mod parser;
mod semant;
mod syntax;

use asm::*;
use parser::*;
use semant::*;
use std::fs::*;
use std::io::Read;

fn usage() {
    println!("Usage: sahl <filename> <option> <verbose>");
    println!("Options:");
    println!("  -c: Compile to bytecode");
    println!("  -e: Execute code on rust backend");
    println!("  -n: Compile to native code");
    println!("Verbose:");
    println!("  -v: Verbose mode");
}

fn main() {
    let filename = std::env::args().nth(1);
    let opt = std::env::args().nth(2);
    let opt2 = std::env::args().nth(3);
    if filename.is_some() && opt.is_some() {
        let to_exec = opt.clone().unwrap() == "-e";
        let to_compile = opt.clone().unwrap() == "-c";
        let native = opt.unwrap() == "-n";
        let verbose = opt2.clone().is_some() && opt2.clone().unwrap() == "-v";
        if !to_exec && !to_compile && !native {
            usage();
            return;
        }
        let mut source = String::new();
        let file = File::open(filename.unwrap());
        match file {
            Ok(mut f) => {
                f.read_to_string(&mut source).unwrap();
            }
            Err(_) => {
                println!("Could not open file: {}", std::env::args().nth(1).unwrap());
                return;
            }
        }

        let res = program(&source);
        match res {
            Ok((_, p)) => {
                if verbose {
                    println!("{:#?}", p);
                }
                let res = check_program(&p);

                match res {
                    Ok((env, typrog)) => {
                        if verbose {
                            println!("Program is well-typed");
                        }
                        if to_compile {
                            let mut codebyte = bytecode::Bytecode::new();
                            codebyte.compile_program(&p);
                            codebyte.write("exe.bin");
                        } else {
                            let mut asm = Asm::new(env);
                            asm.compile(&p);
                        }
                    }
                    Err(e) => {
                        println!("Program is not well-typed: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    } else {
        usage();
    }
}

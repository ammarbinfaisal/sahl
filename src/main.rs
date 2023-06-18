mod asm;
mod bytecode;
mod cfg;
mod go;
mod parser;
mod regcode;
mod semant;
mod syntax;
mod utils;

use asm::*;
use parser::*;
use semant::*;
use std::fs::*;
use std::io::Read;
use std::process::exit;

use crate::cfg::construct_cfg;

fn usage() {
    println!("Usage: sahl <filename> <option> <verbose>");
    println!("Options:");
    println!("  -c: Compile to bytecode");
    println!("  -n: Compile to native code");
    println!("Verbose:");
    println!("  -v: Verbose mode");
}

fn main() {
    let filename = std::env::args().nth(1);
    let opt = std::env::args().nth(2);
    let opt2 = std::env::args().nth(3);
    if filename.is_some() && opt.is_some() {
        let to_go = opt.clone().unwrap() == "-g";
        let to_compile = opt.clone().unwrap() == "-c";
        let native = opt.unwrap() == "-n";
        let verbose = opt2.clone().is_some() && opt2.clone().unwrap() == "-v";
        if !to_go && !to_compile && !native {
            usage();
            return;
        }
        let mut source = String::new();
        let file = File::open(filename.clone().unwrap());
        match file {
            Ok(mut f) => {
                f.read_to_string(&mut source).unwrap();
            }
            Err(_) => {
                println!("Could not open file: {}", std::env::args().nth(1).unwrap());
                return;
            }
        }

        // vbankulkm <error>
        //           ^^^^^^
        let show_error_line = |idx: usize, source: &str| {
            if idx == 0 {
                return;
            }
            let mut line = 1;
            let mut col = 1;
            let mut idx2 = 0;
            for c in source.chars() {
                if c == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                idx2 += 1;
                if idx2 == idx {
                    break;
                }
            }
            let mut col2 = col;
            for c in source.chars().skip(idx2) {
                if c == '\n' {
                    break;
                }
                col2 += 1;
            }
            println!("{}:{}", line, col);
            println!("{}", source.lines().nth(line - 1).unwrap());
            println!("{}^{}", " ".repeat(col - 1), "^".repeat(col2 - col));
        };

        let res = program(&source);
        match res {
            Ok((_, mut p)) => {
                let res = check_program(&mut p);

                match res {
                    Ok(env) => {
                        if verbose {
                            let mut regcodegen = regcode::RegCodeGen::new(source.clone());
                            regcodegen.compile_program(&p);
                            for funcs in regcodegen.func_code.iter() {
                                for instr in funcs.iter().enumerate() {
                                    println!("\t{}: {:?}", instr.0, instr.1);
                                }
                            }
                            println!("CFG:");
                            for funcs in regcodegen.func_code.iter() {
                                for (idx, cfg) in construct_cfg(funcs).iter().enumerate() {
                                    println!("\t{}: {:?}", idx, cfg);
                                }
                            }
                            println!("Program is well-typed");
                        }
                        if to_go {
                            let res = go::compile_program(&p);
                            println!("{}", res)
                        } else if to_compile {
                            let mut codebyte =
                                bytecode::Bytecode::new(filename.unwrap().to_string());
                            codebyte.compile_program(&p);
                            codebyte.write("exe.bin");
                        } else {
                            let mut asm = Asm::new(env);
                            asm.compile(&p);
                        }
                    }
                    Err(e) => {
                        show_error_line(e.0, &source);
                        println!("{}", e.1);
                        exit(1)
                    }
                }
            }
            Err(e) => {
                e.map(|errr| {
                    show_error_line(errr.idx, &source);
                    if let ErrorParse::Unconsumed(s) = errr.error {
                        println!("Failed to parse the below code:");
                        println!("{}", s);
                    }
                });
                exit(1)
            }
        }
    } else {
        usage();
    }
}

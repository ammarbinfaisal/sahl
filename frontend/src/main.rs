#![recursion_limit = "256"]

mod bytes;
mod cfg;
mod go;
mod native;
mod parser;
mod regcode;
mod semant;
mod syntax;
mod utils;

use ariadne::{Color, Label, Report, ReportKind, Source};
use inkwell::context::Context;
use parser::*;
use regcode::RegCodeGen;
use semant::*;
use std::fs::*;
use std::io::{Read, Write};

use crate::bytes::{consts_vec, emit_bytes};

fn usage() {
    println!("Usage: sahl <filename> <option> <verbose>");
    println!("Options:");
    println!("  -c: Compile to bytecode");
    println!("  -n: Compile to native code");
    println!("Verbose:");
    println!("  -v: Verbose mode");
}

fn exec(source: &str, f: &str, to_go: bool, to_compile: bool, verbose: bool) {
    let res = program(source);
    match res {
        Some(mut p) => {
            // println!("{:#?}", p);

            let res2 = check_program(&mut p);

            match res2 {
                Ok(_env) => {
                    if verbose {
                        println!("Program is well-typed");
                    }
                    if to_go {
                        let res = go::compile_program(&p);
                        println!("{}", res)
                    } else {
                        // println!("CFG:");
                        let mut gen = RegCodeGen::new(f.to_string());
                        gen.compile_program(&p, to_compile);
                        if verbose {
                            for funcs in gen.func_code.iter() {
                                for instr in funcs.iter().enumerate() {
                                    println!("\t{}: {:?}", instr.0, instr.1);
                                }
                            }
                        }
                        if to_compile {
                            let main_idx = gen.start_func_idx;
                            let consts = consts_vec(&gen.consts);
                            let mut file = File::create("exe.bin").unwrap();
                            file.write_all(&main_idx.to_le_bytes()).unwrap();
                            file.write_all(&gen.consts.len().to_le_bytes()).unwrap();
                            file.write_all(&consts).unwrap();
                            file.write_all(&gen.func_code.len().to_le_bytes()).unwrap();
                            for func in gen.func_code.iter() {
                                let func_bytes = emit_bytes(func);
                                // println!("func_bytes_len {}", func_bytes.len());
                                file.write_all(&func_bytes.len().to_le_bytes()).unwrap();
                                file.write_all(&func_bytes).unwrap();
                            }
                        } else {
                            let context = Context::create();
                            let module = context.create_module("main");
                            let builder = context.create_builder();
                            let mut compiler = native::Compiler::new(
                                &context,
                                module,
                                builder,
                                gen.consts.clone(),
                            );
                            compiler.compile_program(&p, gen.func_code);
                        }
                    }
                }
                Err(e) => {
                    // seman error
                    // (usize, Error, usize)
                    let out = Color::Fixed(81);
                    Report::build(ReportKind::Error, "source", e.0)
                        .with_code(3)
                        .with_message(format!("Semantic Check"))
                        .with_label(
                            Label::new((f, e.0..e.2))
                                .with_message(e.1)
                                .with_color(out),
                        )
                        .finish()
                        .print((f, Source::from(source)))
                        .unwrap();
                }
            }
        }
        None => {}
    }
}

fn main() {
    let filename = std::env::args().nth(1);
    let opt = std::env::args().nth(2);
    let opt2 = std::env::args().nth(3);
    if filename.is_some() && opt.is_some() {
        let _f = filename.clone().unwrap();
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
                exec(
                    source.as_str(),
                    filename.unwrap().as_str(),
                    to_go,
                    to_compile,
                    verbose,
                );
            }
            Err(_) => {
                println!("Could not open file: {}", std::env::args().nth(1).unwrap());
                return;
            }
        }
    } else {
        usage();
    }
}

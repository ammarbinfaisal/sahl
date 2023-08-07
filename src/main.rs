mod bytes;
mod cfg;
mod go;
mod parser;
mod regcode;
mod semant;
mod syntax;

use ariadne::{Color, Label, Report, ReportKind, Source};
use parser::*;
use regcode::RegCodeGen;
use semant::*;
use std::fs::*;
use std::io::{Read, Write};

use crate::bytes::{consts_vec, emit_bytes};
use crate::cfg::*;

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
        let f = filename.clone().unwrap();
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

        let res = program(&source);
        match res {
            Ok((_, mut p)) => {
                let res = check_program(&mut p);

                match res {
                    Ok(_env) => {
                        if verbose {
                            println!("Program is well-typed");
                        }
                        if to_go {
                            let res = go::compile_program(&p);
                            println!("{}", res)
                        } else {
                            println!("CFG:");
                            let mut gen = RegCodeGen::new(source.clone());
                            gen.compile_program(&p);
                            for funcs in gen.func_code.iter() {
                                for instr in funcs.iter().enumerate() {
                                    println!("\t{}: {:?}", instr.0, instr.1);
                                }
                            }
                            for funcs in gen.func_code.iter() {
                                let cfg = construct_cfg(funcs);
                                let cfg_nodes = construct_cfg_nodes(&cfg, cfg.len());
                                for (idx, cfg) in cfg_nodes.iter().zip(cfg.iter()).enumerate() {
                                    println!("\t{}: {:?} {:?}", idx, cfg.0, cfg.1);
                                }
                            }
                            if to_compile {
                                let main_idx = gen.start_func_idx;
                                let consts = consts_vec(&gen.consts);
                                let mut file = File::create("exe.bin").unwrap();
                                file.write_all(&main_idx.to_le_bytes()).unwrap();
                                file.write_all(&gen.consts.len().to_le_bytes()).unwrap();
                                println!("consts count {}", consts.len());
                                file.write_all(&consts).unwrap();
                                file.write_all(&gen.func_code.len().to_le_bytes()).unwrap();
                                for func in gen.func_code.iter() {
                                    let func_bytes = emit_bytes(func);
                                    println!("func_bytes_len {}", func_bytes.len());
                                    file.write_all(&func_bytes.len().to_le_bytes()).unwrap();
                                    file.write_all(&func_bytes).unwrap();
                                }
                            } else {
                                // let mut asm = Asm::new(env);
                                // asm.compile(&p);
                            }
                        }
                    }
                    Err(e) => {
                        // seman error
                        // (usize, Error, usize)
                        let out = Color::Fixed(81);
                        Report::build(ReportKind::Error, f.clone(), e.0)
                            .with_code(3)
                            .with_message(format!("Semantic Check"))
                            .with_label(
                                Label::new((f.clone(), e.0..e.2))
                                    .with_message(e.1)
                                    .with_color(out),
                            )
                            .finish()
                            .print((f, Source::from(source.as_str())))
                            .unwrap();
                    }
                }
            }
            Err(e) => {
                let out = Color::Fixed(81);
                let e = match e {
                    nom::Err::Incomplete(e) => {
                        println!("Incomplete: {:?}", e);
                        return;
                    }
                    nom::Err::Error(e) => e,
                    nom::Err::Failure(e) => e,
                };
                let e = match e {
                    nom::error::Error { input, code } => (input, code),
                };
                let loc = e.0.location_offset();
                let errstr = e.1.description();
                Report::build(ReportKind::Error, f.clone(), loc)
                    .with_code(3)
                    .with_message(format!("Parsing error"))
                    .with_label(
                        Label::new((f.clone(), loc..loc + 10))
                            .with_message(errstr)
                            .with_color(out),
                    )
                    .finish()
                    .print((f, Source::from(source.as_str())))
                    .unwrap();
            }
        }
    } else {
        usage();
    }
}

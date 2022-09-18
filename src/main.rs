mod syntax;
mod parser;
mod semant;
mod code;
mod vm;

use parser::*;
use semant::*;
use std::fs::*;
use std::io::{Read};

fn main() {
    let filename = std::env::args().nth(1);
    if filename.is_some() {
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
                println!("{:#?}", p);
                let res = check_program(&p);
                match res {
                    Ok(_) => {
                        println!("Program is well-typed");
                        let mut codegen = code::Codegen::new();
                        codegen.compile_program(&p);
                        codegen.execute();
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
    }
}

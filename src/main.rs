mod syntax;
mod parser;

use parser::*;
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
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}

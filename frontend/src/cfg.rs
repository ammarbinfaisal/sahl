use std::collections::{HashMap, HashSet};

use crate::regcode::{RegCode, SuperInstruction};

pub type CFG = Vec<BasicBlock>;

#[derive(Debug)]
pub struct BasicBlock {
    pub code: Vec<RegCode>,
}

pub fn construct_cfg(regcode: &Vec<RegCode>) -> CFG {
    let mut cfg: CFG = Vec::new();
    let mut block: Vec<RegCode> = Vec::new();
    let mut map = HashMap::new(); // regcode index -> block index, code index
    let mut leaders = HashSet::new();

    for code in regcode.into_iter() {
        match code {
            RegCode::Jmp(ix) => {
                leaders.insert(*ix);
            }
            RegCode::JmpIfNot(_, ix) => {
                leaders.insert(*ix);
            }
            RegCode::Super(SuperInstruction::JmpIfNotCond(ix, _, _, _, _)) => {
                leaders.insert(*ix);
            }
            _ => {}
        }
    }

    for (i, code) in regcode.into_iter().enumerate() {
        // if the last code is a jump if not then the next code is a leader
        if let RegCode::JmpIfNot(_, _) = code {
            leaders.insert(i + 1);
        }
        if leaders.contains(&i) && !block.is_empty() {
            cfg.push(BasicBlock {
                code: block,
            });
            block = Vec::new();
        }
        block.push(code.clone());
        map.insert(i, cfg.len());
    }

    if !block.is_empty() {
        cfg.push(BasicBlock {
            code: block,
        });
    }

    // patching jmps
    for block in cfg.iter_mut() {
        for code in block.code.iter_mut() {
            match code {
                RegCode::Jmp(addr) => {
                    let block_idx = map[addr];
                    *code = RegCode::Jmp(block_idx);
                }
                RegCode::JmpIfNot(cond, addr) => {
                    let block_idx = map[addr];
                    *code = RegCode::JmpIfNot(cond.clone(), block_idx);
                }
                RegCode::Super(SuperInstruction::JmpIfNotCond(addr, r1, r2, r3, op)) => {
                    let block_idx = map[addr];
                    *code = RegCode::Super(SuperInstruction::JmpIfNotCond(
                        block_idx,
                        *r1,
                        *r2,
                        *r3,
                        op.clone(),
                    ));
                }
                _ => {}
            }
        }
    }
    // println!("cfg {:?}", cfg);
    cfg
}

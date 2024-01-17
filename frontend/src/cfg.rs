use std::collections::{HashMap, HashSet};

use crate::regcode::{RegCode, SuperInstruction};

pub type CFG<'src> = Vec<BasicBlock<'src>>;

#[derive(Debug)]
pub struct BasicBlock<'src> {
    pub code: Vec<RegCode<'src>>,
}

pub fn construct_cfg<'src>(regcode: &Vec<RegCode<'src>>) -> CFG<'src> {
    let mut cfg: CFG = Vec::new();
    let mut block: Vec<RegCode> = Vec::new();
    let mut map = HashMap::new(); // regcode index -> block index, code index
    let mut leaders = HashSet::new();

    leaders.insert(0);

    for (curr_ix, code) in regcode.into_iter().enumerate() {
        match code {
            RegCode::Jmp(ix) => {
                leaders.insert(*ix);
                leaders.insert(curr_ix + 1);
            }
            RegCode::JmpIfNot(_, ix) => {
                leaders.insert(*ix);
                leaders.insert(curr_ix + 1);
            }
            RegCode::Super(SuperInstruction::JmpIfNotCond(ix, _, _, _, _)) => {
                leaders.insert(*ix);
                leaders.insert(curr_ix + 1);
            }
            _ => {}
        }
    }

    for (i, code) in regcode.into_iter().enumerate() {
        if leaders.contains(&i) && !block.is_empty() {
            cfg.push(BasicBlock { code: block });
            block = Vec::new();
        }
        block.push(code.clone());
        map.insert(i, cfg.len());
    }

    if !block.is_empty() {
        cfg.push(BasicBlock { code: block });
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

    cfg
}

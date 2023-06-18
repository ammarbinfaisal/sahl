use std::collections::{HashMap, HashSet};

use crate::regcode::RegCode;

pub type CFG = Vec<Vec<RegCode>>;

pub fn construct_cfg(regcode: &Vec<RegCode>) -> CFG {
    let mut cfg: CFG = Vec::new();
    let mut block: Vec<RegCode> = Vec::new();
    let mut map = HashMap::new(); // regcode index -> block index, code index
    let mut leaders = HashSet::new();
    for (i, code) in regcode.into_iter().enumerate() {
        if leaders.contains(&i) {
            cfg.push(block);
            block = Vec::new();
        }
        match code {
            RegCode::Jmp(ix) => {
                leaders.insert(i + 1);
                leaders.insert(*ix);
                map.insert(i, cfg.len());
                block.push(code.clone());
            }
            RegCode::JmpIfNot(_, ix) => {
                leaders.insert(i + 1);
                leaders.insert(*ix);
                map.insert(i, cfg.len());
                block.push(code.clone());
            }
            _ => {
                map.insert(i, cfg.len());
                block.push(code.clone());
            }
        }
    }
    cfg.push(block);
    // patching jmps
    for block in cfg.iter_mut() {
        for code in block.iter_mut() {
            match code {
                RegCode::Jmp(addr) => {
                    let block_idx = map[addr];
                    *code = RegCode::Jmp(block_idx);
                }
                RegCode::JmpIfNot(cond, addr, ) => {
                    let block_idx = map[addr];
                    *code = RegCode::JmpIfNot(cond.clone(), block_idx);
                }
                _ => {}
            }
        }
    }
    cfg
}
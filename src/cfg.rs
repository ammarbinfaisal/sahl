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
                RegCode::JmpIfNot(cond, addr) => {
                    let block_idx = map[addr];
                    *code = RegCode::JmpIfNot(cond.clone(), block_idx);
                }
                _ => {}
            }
        }
    }
    cfg
}

pub type Preds = Vec<Vec<usize>>;

pub fn construct_cfg_nodes(cfg: &CFG, len: usize) -> Preds {
    let mut nodes: Preds = vec![Vec::new(); len];
    for (i, block) in cfg.iter().enumerate() {
        let last_idx = block.len() - 1;
        match block[last_idx] {
            RegCode::Jmp(ix) => {
                nodes[ix].push(i);
            }
            RegCode::JmpIfNot(_, ix) => {
                nodes[ix].push(i);
                nodes[i + 1].push(i);
            }
            _ => {
                if i < len - 1 {
                    nodes[i + 1].push(i);
                }
            }
        }
    }
    nodes
}

// control flow is linear except for jumps
pub fn construct_dominators(cfg: &CFG, cfg_nodes: &Preds) -> Vec<HashSet<usize>> {
    let mut doms: Vec<HashSet<usize>> = vec![HashSet::new(); cfg.len()];
    doms[0].insert(0);
    let len = cfg.len();
    for i in 1..len {
        doms[i] = (0..len).collect();
    }
    let mut changed = true;
    while changed {
        changed = false;
        for i in 1..len {
            if cfg_nodes[i].len() == 0 {
                doms[i].clear();
                continue;
            }
            let mut preds = Vec::new();
            for pred in cfg_nodes[i].iter() {
                preds.push(doms[*pred].clone());
            }
            let mut new_dom = preds.iter().skip(1).fold(preds[0].clone(), |acc, x| {
                acc.intersection(&x).cloned().collect()
            });
            new_dom.insert(i);
            if new_dom != doms[i] {
                doms[i] = new_dom;
                changed = true;
            }
        }
    }
    doms
}

pub fn construct_dominance_frontiers(
    cfg: &CFG,
    cfg_nodes: &Preds,
    doms: &Vec<HashSet<usize>>,
) -> Vec<HashSet<usize>> {
    let mut df: Vec<HashSet<usize>> = vec![HashSet::new(); cfg.len()];
    let edges = cfg_nodes
        .iter()
        .enumerate()
        .flat_map(|(i, preds)| preds.iter().map(move |&pred| (pred, i)));
    for (u, v) in edges {
        let mut queue = vec![u];
        let mut visited = HashSet::new();
        while let Some(w) = queue.pop() {
            if visited.contains(&w) {
                continue;
            }
            visited.insert(w);
            if doms[w].contains(&v) {
                continue;
            }
            df[w].insert(v);
            queue.extend(cfg_nodes[w].iter());
        }
    }
    df
}

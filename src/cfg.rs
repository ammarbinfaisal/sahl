use std::collections::{HashMap, HashSet};

use crate::regcode::{RegCode, SuperInstruction};

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
            RegCode::Super(SuperInstruction::JmpIfNotCond(ix, _, _, _, _)) => {
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

pub type Preds = Vec<Vec<usize>>;

pub fn construct_pred_nodes(cfg: &CFG, len: usize) -> Preds {
    let mut nodes: Preds = vec![Vec::new(); len + 1];
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
                if i < len {
                    nodes[i + 1].push(i);
                }
            }
        }
    }
    nodes
}

pub fn construct_children_nodes(pred_nodes: &Preds) -> Vec<Vec<usize>> {
    let mut nodes: Vec<Vec<usize>> = vec![Vec::new(); pred_nodes.len()];
    for (i, preds) in pred_nodes.iter().enumerate() {
        for pred in preds.iter() {
            nodes[*pred].push(i);
        }
    }
    println!("children nodes:");
    for (i, j) in nodes.iter().enumerate() {
        println!("{}: ", i);
        for k in j.into_iter() {
            println!("\t {}", k);
        }
    }
    nodes
}

// control flow is linear except for jumps
pub fn construct_dom_tree(cfg: &CFG, pred_nodes: &Preds) -> Vec<HashSet<usize>> {
    let mut dom_tree: Vec<HashSet<usize>> = vec![HashSet::new(); cfg.len()];
    dom_tree[0].insert(0);
    for (i, _) in cfg.iter().enumerate() {
        let mut dom = HashSet::new();
        for pred in pred_nodes[i].iter() {
            if dom.is_empty() {
                dom = dom_tree[*pred].clone();
            } else {
                dom = dom.intersection(&dom_tree[*pred]).cloned().collect();
            }
        }
        dom.insert(i);
        dom_tree[i] = dom;
    }
    dom_tree
}

pub fn construct_idoms(dom_tree: &Vec<HashSet<usize>>) -> Vec<usize> {
    let mut idoms = vec![0; dom_tree.len()];
    for (i, dom) in dom_tree.iter().enumerate() {
        for j in dom.iter() {
            if i != *j {
                idoms[i] = *j;
            }
        }
    }
    idoms
}

fn toposort_dfs(
    node: usize,
    children_nodes: &Vec<Vec<usize>>,
    visited: &mut Vec<bool>,
    sorted: &mut Vec<usize>,
) {
    visited[node] = true;
    for child in children_nodes[node].iter() {
        if !visited[*child] {
            toposort_dfs(*child, children_nodes, visited, sorted);
        }
    }
    sorted.push(node);
}

fn toposort(children_nodes: &Vec<Vec<usize>>) -> Vec<usize> {
    let mut visited = vec![false; children_nodes.len()];
    let mut sorted = Vec::new();
    for (i, _) in children_nodes.iter().enumerate() {
        if !visited[i] {
            toposort_dfs(i, children_nodes, &mut visited, &mut sorted);
        }
    }
    sorted.reverse();
    sorted
}

// for each X in bottom-up traversal of dominator tree
//     DF(X) = { }
//     for Y in parent of X
//         if idom(Y) != X then
//             DF(X) = DF(X) union {Y}
//         end
//     end
//     for Z in children of X
//         for Y in DF(Z)
//             if idom(Y) != X then
//                 DF(X) = DF(X) union {Y}
//             end
//         end
//     end
// end
pub fn construct_dominance_frontiers(
    pred_nodes: &Preds,
    idoms: &Vec<usize>,
) -> Vec<HashSet<usize>> {
    let children_nodes = construct_children_nodes(pred_nodes);
    let bottom_up = toposort(&children_nodes);
    let mut df = vec![HashSet::new(); idoms.len() + 1];
    for x in bottom_up.iter() {
        for y in pred_nodes[*x].iter() {
            if idoms[*y] != *x {
                df[*x].insert(*y);
            }
        }
        for z in children_nodes[*x].iter() {
            let dfz = &df[*z];
            for y in dfz.clone().iter() {
                if idoms[*y] != *x {
                    df[*x].insert(*y);
                }
            }
        }
    }
    df
}

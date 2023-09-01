use std::collections::{HashMap, HashSet};

use crate::regcode::{RegCode, SuperInstruction};

pub type CFG = Vec<BasicBlock>;

#[derive(Debug)]
pub struct BasicBlock {
    phi: HashMap<usize, HashMap<usize, usize>>,
    pub code: Vec<RegCode>,
}

pub fn construct_cfg(regcode: &Vec<RegCode>) -> CFG {
    let mut cfg: CFG = Vec::new();
    let mut block: Vec<RegCode> = Vec::new();
    let mut map = HashMap::new(); // regcode index -> block index, code index
    let mut leaders = HashSet::new();

    // find leaders
    for (i, code) in regcode.iter().enumerate() {
        match code {
            RegCode::Jmp(ix) => {
                leaders.insert(*ix);
            }
            RegCode::JmpIfNot(_, ix) => {
                leaders.insert(*ix);
                leaders.insert(i + 1);
            }
            _ => {}
        }
    }

    // construct blocks
    for (i, code) in regcode.iter().enumerate() {
        if leaders.contains(&i) && !block.is_empty() {
            cfg.push(BasicBlock {
                phi: HashMap::new(),
                code: block,
            });
            block = Vec::new();
        }
        block.push(code.clone());
        map.insert(i, cfg.len());
    }

    cfg.push(BasicBlock {
        phi: HashMap::new(),
        code: block,
    });
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
                _ => {}
            }
        }
    }

    // for (i, block) in cfg.iter().enumerate() {
    //     println!("block {}", i);
    //     for code in block.code.iter() {
    //         println!("\t{:?}", code);
    //     }
    // }
    cfg
}

pub fn construct_succs_nodes(cfg: &CFG, len: usize) -> Vec<Vec<usize>> {
    let mut nodes = vec![Vec::new(); len + 1];
    for (i, block) in cfg.iter().enumerate() {
        let last_idx = block.code.len() - 1;
        match block.code[last_idx] {
            RegCode::Jmp(ix) => {
                nodes[i].push(ix);
            }
            RegCode::JmpIfNot(_, ix) => {
                nodes[i].push(ix);
                nodes[i].push(i + 1);
            }
            _ => {
                if i < len {
                    nodes[i].push(i + 1);
                }
            }
        }
    }
    nodes
}

fn succ_to_preds(succ_nodes: &Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    let mut preds = vec![Vec::new(); succ_nodes.len()];
    for (i, succs) in succ_nodes.iter().enumerate() {
        for succ in succs.iter() {
            preds[*succ].push(i);
        }
    }
    preds
}

pub fn construct_children_nodes(dom_tree: &Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    let mut nodes: Vec<Vec<usize>> = vec![Vec::new(); dom_tree.len()];
    for (i, preds) in dom_tree.iter().enumerate() {
        for pred in preds.iter() {
            nodes[*pred].push(i);
        }
    }
    nodes
}

// control flow is linear except for jumps
pub fn construct_revdom_tree(cfg: &CFG, succ_nodes: &Vec<Vec<usize>>) -> Vec<HashSet<usize>> {
    let pred_nodes = succ_to_preds(succ_nodes);
    let mut rev_dom_tree: Vec<HashSet<usize>> = vec![HashSet::new(); cfg.len() + 1];
    rev_dom_tree[0].insert(0);
    for (i, _) in cfg.iter().enumerate() {
        let mut dom = HashSet::new();
        for pred in pred_nodes[i].iter() {
            if dom.is_empty() {
                dom = rev_dom_tree[*pred].clone();
            } else {
                dom = dom.intersection(&rev_dom_tree[*pred]).cloned().collect();
            }
        }
        dom.insert(i);
        rev_dom_tree[i] = dom;
    }
    rev_dom_tree
}

pub fn construct_dom_tree(idoms: &Vec<usize>, succ_nodes: &Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    let order = toposort(succ_nodes);
    let mut dom_tree = vec![Vec::new(); idoms.len()];
    for i in order.iter() {
        if *i == 0 {
            continue;
        }
        dom_tree[idoms[*i]].push(*i);
    }
    dom_tree
}

pub fn construct_idoms(dom_tree: &Vec<HashSet<usize>>) -> Vec<usize> {
    let mut idoms = vec![0; dom_tree.len()];
    for (i, dom) in dom_tree.iter().enumerate() {
        for j in dom.iter() {
            if i != *j && *j > 0 {
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

fn depth_first_preorder(
    node: usize,
    children_nodes: &Vec<Vec<usize>>,
    visited: &mut Vec<bool>,
    sorted: &mut Vec<usize>,
) {
    visited[node] = true;
    sorted.push(node);
    for child in children_nodes[node].iter() {
        if !visited[*child] {
            depth_first_preorder(*child, children_nodes, visited, sorted);
        }
    }
}

fn depth_first_preorder_traversal(children_nodes: &Vec<Vec<usize>>) -> Vec<usize> {
    let mut visited = vec![false; children_nodes.len()];
    let mut sorted = Vec::new();
    for (i, _) in children_nodes.iter().enumerate() {
        if !visited[i] {
            depth_first_preorder(i, children_nodes, &mut visited, &mut sorted);
        }
    }
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
    succ_nodes: &Vec<Vec<usize>>,
    dom_tree: &Vec<Vec<usize>>,
    idoms: &Vec<usize>,
) -> Vec<HashSet<usize>> {
    let children_nodes = construct_children_nodes(dom_tree);
    let bottom_up = toposort(&children_nodes);
    // println!("bottom_up {:?}", bottom_up);
    let mut df = vec![HashSet::new(); idoms.len()];
    for x in bottom_up.iter() {
        // println!("x {}", x);
        for y in succ_nodes[*x].iter() {
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

fn get_defs(cfg: &CFG, var_count: usize) -> Vec<Vec<(usize, usize)>> {
    let mut defs = vec![Vec::new(); var_count + 1];
    for (i, block) in cfg.iter().enumerate() {
        for (j, code) in block.code.iter().enumerate() {
            match code {
                RegCode::Store(r, _, _) => {
                    defs[*r].push((i, j));
                }
                _ => {}
            }
        }
    }
    defs
}

pub fn insert_phi_functions(cfg: &mut CFG, df: &Vec<HashSet<usize>>, var_count: usize) {
    let mut defs = get_defs(cfg, var_count);
    let node_count = cfg.len();
    for var in 0..var_count {
        let mut worklist = defs[var]
            .iter()
            .map(|(i, _)| *i)
            .collect::<HashSet<usize>>();
        let mut inserted = HashSet::new();
        loop {
            let i = worklist.iter().next();
            let i = match i {
                Some(i) => *i,
                None => break,
            };
            worklist.remove(&i);
            for j in df[i].iter() {
                if !inserted.contains(j) {
                    inserted.insert(*j);
                    let mut phi = Vec::new();
                    for (k, _) in defs[var].iter() {
                        phi.push(*k);
                    }
                    if *j >= node_count {
                        continue;
                    }
                    cfg[*j].phi.insert(var, phi.into_iter().map(|i| (i, 0)).collect());
                    if !defs[var].contains(&(*j, 0)) {
                        defs[var].push((*j, 0));
                        worklist.insert(*j);
                    }
                }
            }
        }
    }
}

// dfs
pub fn rename_variable(
    node: usize,
    cfg: &mut CFG,
    dom_tree: &Vec<Vec<usize>>,
    vmap: &mut HashMap<usize, usize>,
    visited: &mut Vec<bool>,
) {
    for code in cfg[node].code.iter_mut() {
        match code {
            RegCode::Load(r, _, v) => {
                if vmap.contains_key(r) {
                    *v = vmap[r];
                } else {
                    vmap.insert(*r, 0);
                }
            }
            RegCode::Store(r, _, v) => {
                if vmap.contains_key(r) {
                    *v = vmap[r] + 1;
                    let vmap_r_mut = vmap.get_mut(r).unwrap();
                    *vmap_r_mut = *v;
                } else {
                    *v = 0;
                    vmap.insert(*r, 0);
                }
            }
            _ => {}
        }
    }
    for succ in dom_tree[node].iter() {
        if *succ < cfg.len() &&  !visited[*succ] {
            visited[*succ] = true;
            // updagte the phi nodes in succ block
            let phis = &mut cfg[*succ].phi; 
            for (var, phi) in phis.iter_mut() {
                for (i, j) in phi.iter_mut() {
                    if *i == node {
                        if let Some(v) = vmap.get(var) {
                            *j = *v;
                        }
                    }
                }
            }
            rename_variable(*succ, cfg, dom_tree, vmap, visited);
        }
    }
}

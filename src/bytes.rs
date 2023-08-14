use std::{collections::HashMap, vec};

use crate::{regcode::*, syntax::Type};

const IADD: u8 = 0;
const ISUB: u8 = 1;
const IMUL: u8 = 2;
const IDIV: u8 = 3;
const IREM: u8 = 4;
const INE: u8 = 5;
const IEQ: u8 = 6;
const ILT: u8 = 7;
const ILE: u8 = 8;
const IGT: u8 = 9;
const IGE: u8 = 10;
const FADD: u8 = 11;
const FSUB: u8 = 12;
const FMUL: u8 = 13;
const FDIV: u8 = 14;
const FREM: u8 = 15;
const FNE: u8 = 16;
const FEQ: u8 = 17;
const FLT: u8 = 18;
const FLE: u8 = 19;
const FGT: u8 = 20;
const FGE: u8 = 21;
const BAND: u8 = 22;
const BOR: u8 = 23;
const BXOR: u8 = 24;
const BNOT: u8 = 25;
const LAND: u8 = 26;
const LOR: u8 = 27;
const LNOT: u8 = 28;
const BSHL: u8 = 29;
const BSHR: u8 = 30;
const FNEG: u8 = 31;
const INEG: u8 = 32;
const MAKE: u8 = 33;
const LISTSET: u8 = 34;
const LISTGET: u8 = 35;
const LIST: u8 = 36;
const TUPLEGET: u8 = 37;
const TUPLE: u8 = 38;
const STRGET: u8 = 39;
const MAPGET: u8 = 40;
const MAPSET: u8 = 41;
const CHANSEND: u8 = 42;
const CHANRECV: u8 = 43;
const JMP: u8 = 44;
const JMPNOT: u8 = 45;
const CALL: u8 = 46;
const NCALL: u8 = 47;
const CONST: u8 = 48;
const LOAD: u8 = 49;
const STORE: u8 = 50;
const CAST: u8 = 51;
const MOVE: u8 = 52;
const RETURN: u8 = 53;
const PUSH: u8 = 54;
const POP: u8 = 55;
const SPAWN: u8 = 56;
const NOP: u8 = 57;
const RET: u8 = 58;
const STACKMAP: u8 = 59;

fn rec_vectorise_ty(ty: &Type, vec: &mut Vec<u8>) {
    match ty {
        Type::Int => vec.push(0),
        Type::Double => vec.push(1),
        Type::Bool => vec.push(2),
        Type::Char => vec.push(3),
        Type::Str => vec.push(4),
        Type::List(_ty) => {
            vec.push(5);
            // rec_vectorise_ty(ty, vec);
        }
        Type::Map(_ty1, _ty2) => {
            vec.push(6);
            // rec_vectorise_ty(ty1, vec);
            // rec_vectorise_ty(ty2, vec);
        }
        Type::Chan(_ty) => {
            vec.push(7);
        }
        Type::Tuple(_tys) => {
            vec.push(8);
            // vec.push(tys.len() as u8);
            // for ty in tys {
            //     rec_vectorise_ty(ty, vec);
            // }
        }
        _ => {}
    }
}

pub fn emit_bytes(code: &Vec<RegCode>) -> Vec<u8> {
    let mut bytes = Vec::new();
    let mut ixmap = HashMap::new();
    let mut jumps = Vec::new();

    for (i, c) in code.iter().enumerate() {
        ixmap.insert(i, bytes.len());
        match c {
            RegCode::IAdd(r1, r2, res) => {
                bytes.extend(vec![IADD, *r1, *r2, *res]);
            }
            RegCode::ISub(r1, r2, res) => {
                bytes.extend(vec![ISUB, *r1, *r2, *res]);
            }
            RegCode::IMul(r1, r2, res) => {
                bytes.extend(vec![IMUL, *r1, *r2, *res]);
            }
            RegCode::IDiv(r1, r2, res) => {
                bytes.extend(vec![IDIV, *r1, *r2, *res]);
            }
            RegCode::IRem(r1, r2, res) => {
                bytes.extend(vec![IREM, *r1, *r2, *res]);
            }
            RegCode::INe(r1, r2, res) => {
                bytes.extend(vec![INE, *r1, *r2, *res]);
            }
            RegCode::IEq(r1, r2, res) => {
                bytes.extend(vec![IEQ, *r1, *r2, *res]);
            }
            RegCode::ILt(r1, r2, res) => {
                bytes.extend(vec![ILT, *r1, *r2, *res]);
            }
            RegCode::ILe(r1, r2, res) => {
                bytes.extend(vec![ILE, *r1, *r2, *res]);
            }
            RegCode::IGt(r1, r2, res) => {
                bytes.extend(vec![IGT, *r1, *r2, *res]);
            }
            RegCode::IGe(r1, r2, res) => {
                bytes.extend(vec![IGE, *r1, *r2, *res]);
            }
            RegCode::FAdd(r1, r2, res) => {
                bytes.extend(vec![FADD, *r1, *r2, *res]);
            }
            RegCode::FSub(r1, r2, res) => {
                bytes.extend(vec![FSUB, *r1, *r2, *res]);
            }
            RegCode::FMul(r1, r2, res) => {
                bytes.extend(vec![FMUL, *r1, *r2, *res]);
            }
            RegCode::FDiv(r1, r2, res) => {
                bytes.extend(vec![FDIV, *r1, *r2, *res]);
            }
            RegCode::FRem(r1, r2, res) => {
                bytes.extend(vec![FREM, *r1, *r2, *res]);
            }
            RegCode::FNe(r1, r2, res) => {
                bytes.extend(vec![FNE, *r1, *r2, *res]);
            }
            RegCode::FEq(r1, r2, res) => {
                bytes.extend(vec![FEQ, *r1, *r2, *res]);
            }
            RegCode::FLt(r1, r2, res) => {
                bytes.extend(vec![FLT, *r1, *r2, *res]);
            }
            RegCode::FLe(r1, r2, res) => {
                bytes.extend(vec![FLE, *r1, *r2, *res]);
            }
            RegCode::FGt(r1, r2, res) => {
                bytes.extend(vec![FGT, *r1, *r2, *res]);
            }
            RegCode::FGe(r1, r2, res) => {
                bytes.extend(vec![FGE, *r1, *r2, *res]);
            }
            RegCode::BAnd(r1, r2, res) => {
                bytes.extend(vec![BAND, *r1, *r2, *res]);
            }
            RegCode::BOr(r1, r2, res) => {
                bytes.extend(vec![BOR, *r1, *r2, *res]);
            }
            RegCode::BXor(r1, r2, res) => {
                bytes.extend(vec![BXOR, *r1, *r2, *res]);
            }
            RegCode::BNot(r1, res) => {
                bytes.extend(vec![BNOT, *r1, *res]);
            }
            RegCode::LAnd(r1, r2, res) => {
                bytes.extend(vec![LAND, *r1, *r2, *res]);
            }
            RegCode::LOr(r1, r2, res) => {
                bytes.extend(vec![LOR, *r1, *r2, *res]);
            }
            RegCode::LNot(r1, res) => {
                bytes.extend(vec![LNOT, *r1, *res]);
            }
            RegCode::BShl(r1, r2, res) => {
                bytes.extend(vec![BSHL, *r1, *r2, *res]);
            }
            RegCode::BShr(r1, r2, res) => {
                bytes.extend(vec![BSHR, *r1, *r2, *res]);
            }
            RegCode::FNeg(r1, res) => {
                bytes.extend(vec![FNEG, *r1, *res]);
            }
            RegCode::INeg(r1, res) => {
                bytes.extend(vec![INEG, *r1, *res]);
            }
            RegCode::Make(ty, res, def) => {
                let mut tyvec = Vec::new();
                rec_vectorise_ty(ty, &mut tyvec);
                bytes.extend(vec![MAKE, *res, *def]);
                bytes.extend(tyvec);
            }
            RegCode::ListSet(ls_reg, idx_reg, val_reg) => {
                bytes.extend(vec![LISTSET, *ls_reg, *idx_reg, *val_reg]);
            }
            RegCode::ListGet(ls_reg, idx_reg, res_reg) => {
                bytes.extend(vec![LISTGET, *ls_reg, *idx_reg, *res_reg]);
            }
            RegCode::List(len, res) => {
                let mut opcodes = vec![LIST];
                opcodes.extend(len.to_le_bytes().iter());
                opcodes.extend(vec![*res]);
                bytes.extend(opcodes);
            }
            RegCode::TupleGet(tup_reg, idx_reg, res_reg) => {
                bytes.extend(vec![TUPLEGET, *tup_reg, *idx_reg, *res_reg]);
            }
            RegCode::Tuple(len, res) => {
                let mut opcodes = vec![TUPLE];
                opcodes.extend(len.to_le_bytes().iter());
                opcodes.extend(vec![*res]);
                bytes.extend(opcodes);
            }
            RegCode::StrGet(str_reg, idx_reg, res_reg) => {
                bytes.extend(vec![STRGET, *str_reg, *idx_reg, *res_reg]);
            }
            RegCode::MapGet(map_reg, key_reg, res_reg) => {
                bytes.extend(vec![MAPGET, *map_reg, *key_reg, *res_reg]);
            }
            RegCode::MapSet(map_reg, key_reg, val_reg) => {
                bytes.extend(vec![MAPSET, *map_reg, *key_reg, *val_reg]);
            }
            RegCode::ChanSend(chan_var, val_reg) => {
                let mut opcodes = vec![CHANSEND];
                opcodes.extend(chan_var.to_le_bytes().iter());
                opcodes.extend(vec![*val_reg]);
                bytes.extend(opcodes);
            }
            RegCode::ChanRecv(chan_var, res_reg) => {
                let mut opcodes = vec![CHANRECV];
                opcodes.extend(chan_var.to_le_bytes().iter());
                opcodes.extend(vec![*res_reg]);
                bytes.extend(opcodes);
            }
            RegCode::Jmp(ix) => {
                bytes.push(JMP);
                jumps.push((bytes.len(), *ix));
                for _ in 0..8 {
                    bytes.push(0);
                }
            }
            RegCode::JmpIfNot(r1, ix) => {
                bytes.push(JMPNOT);
                bytes.push(*r1);
                jumps.push((bytes.len(), *ix));
                for _ in 0..8 {
                    bytes.push(0);
                }
            }
            RegCode::Call(ix, args) => {
                let mut opcodes = vec![CALL];
                opcodes.extend(ix.to_le_bytes().iter());
                opcodes.extend(args.len().to_le_bytes().iter());
                opcodes.extend(args.iter());
                bytes.extend(opcodes);
            }
            RegCode::NCall(ix, args) => {
                let mut opcodes = vec![NCALL];
                opcodes.push(*ix);
                opcodes.extend(args.len().to_le_bytes().iter());
                opcodes.extend(args.iter());
                bytes.extend(opcodes);
            }
            RegCode::Const(c, res) => {
                let mut opcodes = vec![CONST];
                opcodes.extend(c.to_le_bytes().iter());
                opcodes.push(*res);
                bytes.extend(opcodes);
            }
            RegCode::Load(r1, res) => {
                let mut opcodes = vec![LOAD];
                opcodes.extend(r1.to_le_bytes().iter());
                opcodes.extend(vec![*res]);
                bytes.extend(opcodes);
            }
            RegCode::Store(r1, res) => {
                let mut opcodes = vec![STORE];
                opcodes.extend(r1.to_le_bytes().iter());
                opcodes.extend(vec![*res]);
                bytes.extend(opcodes);
            }
            RegCode::Cast(reg, fromty, toty, res) => {
                let mut opcodes = vec![CAST, *reg];
                let mut tys = Vec::new();
                rec_vectorise_ty(fromty, &mut tys);
                rec_vectorise_ty(toty, &mut tys);
                opcodes.extend(tys);
                opcodes.extend(vec![*res]);
                bytes.extend(opcodes);
            }
            RegCode::Move(res, r) => {
                bytes.extend(vec![MOVE, *res, *r]);
            }
            RegCode::Return(r1) => {
                let mut opcodes = vec![RETURN];
                opcodes.extend(r1.to_le_bytes().iter());
                bytes.extend(opcodes);
            }
            RegCode::Push(r1) => {
                bytes.extend(vec![PUSH, *r1]);
            }
            RegCode::Pop(r1) => {
                bytes.extend(vec![POP, *r1]);
            }
            RegCode::Spawn => {
                bytes.extend(vec![SPAWN]);
            }
            RegCode::StackMap(bitsets) => {
                if bitsets.len() > 0 {
                    let mut opcodes = vec![STACKMAP];
                    opcodes.extend(bitsets.len().to_le_bytes().iter());
                    for bs in bitsets {
                        opcodes.extend(bs.to_le_bytes().iter());
                    }
                    bytes.extend(opcodes);
                }
            }
            RegCode::Nop => {}
            RegCode::Phi(_) => {}
            RegCode::FreeRegs => {}
        }
    }

    // // patch jumps
    for (ix, jumpix) in jumps {
        if !ixmap.contains_key(&jumpix) {
            bytes.splice(ix..ix + 8, bytes.len().to_le_bytes().iter().cloned());
            continue;
        }
        let ix2 = ixmap[&jumpix];
        let bytes2 = ix2.to_le_bytes().to_vec();
        bytes.splice(ix..ix + 8, bytes2.iter().cloned());
        println!("patching jump at {} from {} to {}", ix, jumpix, ix2);
    }

    println!("bytes {:?}", bytes.clone());

    bytes
}

pub fn consts_vec(consts: &Vec<(Type, Vec<u8>)>) -> Vec<u8> {
    let mut cnsts = Vec::new();

    for c in consts.iter() {
        let mut ty = Vec::new();
        rec_vectorise_ty(&c.0, &mut ty);
        cnsts.extend(ty);
        if c.0 == Type::Str {
            cnsts.extend(c.1.len().to_le_bytes().iter());
        }
        cnsts.extend(c.1.iter());
    }

    println!("consts {:?}", cnsts.clone());

    cnsts
}

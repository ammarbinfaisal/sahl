use crate::{
    syntax::*,
    utils::get_literal_type,
};
use std::{collections::HashMap, process::exit};

// highlevel enum for 3/4 address code
#[derive(Debug, Clone)]
pub enum RegCode<'src> {
    // reg1 reg2 res_reg
    IAdd(u8, u8, u8),
    ISub(u8, u8, u8),
    IMul(u8, u8, u8),
    IDiv(u8, u8, u8),
    IRem(u8, u8, u8),
    INe(u8, u8, u8),
    IEq(u8, u8, u8),
    ILt(u8, u8, u8),
    ILe(u8, u8, u8),
    IGt(u8, u8, u8),
    IGe(u8, u8, u8),
    FAdd(u8, u8, u8),
    FSub(u8, u8, u8),
    FMul(u8, u8, u8),
    FDiv(u8, u8, u8),
    FRem(u8, u8, u8),
    FNe(u8, u8, u8),
    FEq(u8, u8, u8),
    FLt(u8, u8, u8),
    FLe(u8, u8, u8),
    FGt(u8, u8, u8),
    FGe(u8, u8, u8),
    BAnd(u8, u8, u8),
    BOr(u8, u8, u8),
    BXor(u8, u8, u8),
    BNot(u8, u8),
    LAnd(u8, u8, u8),
    LOr(u8, u8, u8),
    LNot(u8, u8),
    BShl(u8, u8, u8),
    BShr(u8, u8, u8),
    FNeg(u8, u8),
    INeg(u8, u8),
    // encapulates a make call
    Make(Type<'src>, u8, u8),
    // lists
    ListSet(u8, u8, u8),
    ListGet(u8, u8, u8),
    // List(usize, u8, Type), // not needed as of now
    // tuples
    TupleGet(u8, u8, u8),
    TupleSet(u8, u8, u8),
    Tuple(usize, u8, Vec<Type<'src>>), // length, reg
    // strings
    StrGet(u8, u8, u8),
    // maps
    MapGet(u8, u8, u8),
    MapSet(u8, u8, u8),
    // channels
    ChanSend(usize, u8),
    ChanRecv(usize, u8),
    // control flow
    Jmp(usize),
    JmpIfNot(u8, usize),
    Call(usize, Vec<u8>),
    // others
    NCall(u8, Vec<u8>),
    Const(usize, u8),        // const_idx, reg
    Load(usize, u8, usize),  // local_ix, reg, version
    Store(usize, u8, usize), // local_ix, reg, version
    Cast(u8, Type<'src>, Type<'src>, u8),
    Move(u8, u8),
    Return(u8),
    VoidReturn,
    Push(u8),
    Nop,
    FreeRegs,
    Pop(u8),
    StackMap(Vec<u64>), // set of locals that are live
    Super(SuperInstruction<'src>),
    CoroCall(usize, Vec<u8>),
    Clone(u8, u8, bool),
}

fn is_cond_op(c: &RegCode) -> bool {
    match c {
        RegCode::INe(_, _, _)
        | RegCode::IEq(_, _, _)
        | RegCode::ILt(_, _, _)
        | RegCode::ILe(_, _, _)
        | RegCode::IGt(_, _, _)
        | RegCode::IGe(_, _, _)
        | RegCode::FNe(_, _, _)
        | RegCode::FEq(_, _, _)
        | RegCode::FLt(_, _, _)
        | RegCode::FLe(_, _, _)
        | RegCode::FGt(_, _, _)
        | RegCode::FGe(_, _, _)
        | RegCode::LAnd(_, _, _)
        | RegCode::LOr(_, _, _) => true,
        _ => false,
    }
}

fn three_operand_code(c: &RegCode) -> (u8, u8, u8) {
    match c {
        RegCode::IAdd(r1, r2, r3)
        | RegCode::ISub(r1, r2, r3)
        | RegCode::IMul(r1, r2, r3)
        | RegCode::IDiv(r1, r2, r3)
        | RegCode::IRem(r1, r2, r3)
        | RegCode::INe(r1, r2, r3)
        | RegCode::IEq(r1, r2, r3)
        | RegCode::ILt(r1, r2, r3)
        | RegCode::ILe(r1, r2, r3)
        | RegCode::IGt(r1, r2, r3)
        | RegCode::IGe(r1, r2, r3)
        | RegCode::FAdd(r1, r2, r3)
        | RegCode::FSub(r1, r2, r3)
        | RegCode::FMul(r1, r2, r3)
        | RegCode::FDiv(r1, r2, r3)
        | RegCode::FRem(r1, r2, r3)
        | RegCode::FNe(r1, r2, r3)
        | RegCode::FEq(r1, r2, r3)
        | RegCode::FLt(r1, r2, r3)
        | RegCode::FLe(r1, r2, r3)
        | RegCode::FGt(r1, r2, r3)
        | RegCode::FGe(r1, r2, r3)
        | RegCode::BAnd(r1, r2, r3)
        | RegCode::BOr(r1, r2, r3)
        | RegCode::BXor(r1, r2, r3)
        | RegCode::LAnd(r1, r2, r3)
        | RegCode::LOr(r1, r2, r3)
        | RegCode::BShl(r1, r2, r3)
        | RegCode::BShr(r1, r2, r3) => (*r1, *r2, *r3),
        _ => (0, 0, 0),
    }
}

#[derive(Debug, Clone)]
pub enum SuperInstruction<'src> {
    /// var_ix, const_ix, op_res_reg
    LoadConstOp(usize, usize, u8, Box<RegCode<'src>>),
    /// var_ix, const_ix
    LoadConstOpStore(usize, usize, Box<RegCode<'src>>),
    /// jmp, reg1, reg2, res_reg, cond_op
    JmpIfNotCond(usize, u8, u8, u8, Box<RegCode<'src>>),
}

#[derive(Debug, Clone)]
enum SuperInstParseState<'src> {
    None,
    /// var_ix, reg
    Load(usize, u8),
    /// var_ix, const_ix, load_reg, const_reg
    LoadConst(usize, usize, u8, u8),
    /// var_ix, const_ix, op_res_reg, op
    LoadConstOp(usize, usize, u8, RegCode<'src>),
    /// jmp_ix, reg
    Cond(RegCode<'src>),
}

#[derive(Debug, Clone)]
struct NestedEnv {
    prev_local: usize,
    local_count: Vec<usize>,
    locals: Vec<HashMap<String, usize>>,
    live_vars: HashMap<usize, bool>,
}

impl NestedEnv {
    fn new() -> Self {
        NestedEnv {
            prev_local: 0,
            local_count: Vec::new(),
            locals: vec![HashMap::new()],
            live_vars: HashMap::new(),
        }
    }

    fn push(&mut self) {
        self.local_count.push(self.prev_local);
        self.locals.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.prev_local = self.local_count.pop().unwrap();
        let lcls = self.locals.pop();
        for i in lcls.unwrap().into_iter() {
            self.live_vars.remove(&i.1);
        }
    }

    fn insert(&mut self, name: &str) -> usize {
        let idx = self.prev_local;
        let map = self.locals.last_mut().unwrap();
        map.insert(name.to_string(), idx);
        self.live_vars.insert(idx, false);
        self.prev_local += 1;
        idx
    }

    fn is_live(&self, idx: usize) -> bool {
        let live = self.live_vars.get(&idx);
        if live.is_some() {
            return *live.unwrap();
        }
        false
    }

    fn get(&self, name: &str) -> Option<&usize> {
        for map in self.locals.iter().rev() {
            let idx = map.get(name);
            if idx.is_some() {
                return idx;
            }
        }
        None
    }

    fn set_live_var(&mut self, lcl: usize) {
        let idx = self.live_vars.get_mut(&lcl);
        if idx.is_some() {
            *idx.unwrap() = true;
            return;
        }
    }

    fn clear(&mut self) {
        self.prev_local = 0;
        self.locals.clear();
        self.local_count.clear();
        self.locals.push(HashMap::new());
    }
}

pub struct RegCodeGen<'a, 'src> {
    code: Vec<RegCode<'src>>,
    locals: NestedEnv,
    func_idx: HashMap<&'a str, usize>,
    func_args_len: Vec<u32>,
    typemap: HashMap<&'src str, Type<'src>>,
    variants: HashMap<&'src str, (Type<'src>, &'src str, usize)>,
    constmap: &'a HashMap<&'src str, Spanned<Lit>>,
    curr_func: usize,
    // opcode_span: HashMap<usize, (usize, usize)>, // to be used for debugging
    span: (usize, usize),
    // source_name: String, // to be used for debugging
    stack: Vec<u8>,
    free_regs: [bool; 256],
    breaks: Vec<Vec<usize>>,
    loop_starts: Vec<usize>, // stack of start index of loops
    coro_call: bool,
    pub consts: Vec<(Type<'src>, Vec<u8>)>,
    pub func_code: Vec<Vec<RegCode<'src>>>,
    pub start_func_idx: usize,
}

impl<'a, 'src> RegCodeGen<'a, 'src> {
    pub fn new(
        _source_name: String,
        typemap: HashMap<&'src str, Type<'src>>,
        variants: HashMap<&'src str, (Type<'src>, &'src str, usize)>,
        constmap: &'a HashMap<&'src str, Spanned<Lit>>,
    ) -> Self {
        RegCodeGen {
            code: Vec::new(),
            locals: NestedEnv::new(),
            func_idx: HashMap::new(),
            func_code: Vec::new(),
            func_args_len: Vec::new(),
            loop_starts: Vec::new(),
            start_func_idx: 0,
            curr_func: 0,
            // opcode_span: HashMap::new(),
            consts: Vec::new(),
            span: (0, 0),
            // source_name,
            stack: Vec::new(),
            free_regs: [true; 256],
            breaks: Vec::new(),
            coro_call: false,
            constmap,
            variants,
            typemap,
        }
    }

    fn add_local(&mut self, name: &'a str) -> usize {
        self.locals.insert(name)
    }

    fn get_local(&self, name: &str) -> Option<&usize> {
        self.locals.get(name)
    }

    fn free_reg(&mut self, reg: u8) {
        self.free_regs[reg as usize] = true;
    }

    fn get_reg(&mut self) -> u8 {
        for (i, b) in self.free_regs.iter_mut().enumerate() {
            if i == 0 {
                continue;
            }
            if *b {
                *b = false;
                return i as u8;
            }
        }
        panic!("no free registers");
    }

    // native_ix, arity
    fn builtin(&mut self, name: &str) -> Option<(u8, u8, bool)> {
        match name {
            "append" => Some((5, 2, false)),
            "len" => Some((6, 1, true)),
            "pop" => Some((7, 1, true)),
            _ => None,
        }
    }

    fn stack_pop(&mut self) -> u8 {
        let reg = self.stack.pop().unwrap();
        // println!("stack pop: {}", reg);
        self.free_reg(reg);
        reg
    }

    fn stack_push(&mut self, reg: u8) {
        self.stack.push(reg);
        self.free_regs[reg as usize] = false;
    }

    fn stack_unfree_pop(&mut self) -> u8 {
        let reg = self.stack.pop().unwrap();
        // println!("stack unfree pop: {}", reg);
        reg
    }

    fn compile_print(&mut self, reg: u8, arg_ty: Type) {
        match arg_ty {
            Type::Int => {
                self.code.push(RegCode::NCall(0, vec![reg]));
            }
            Type::Double => {
                self.code.push(RegCode::NCall(1, vec![reg]));
            }
            Type::Char => {
                self.code.push(RegCode::NCall(2, vec![reg]));
            }
            Type::Bool => {
                self.code.push(RegCode::NCall(3, vec![reg]));
            }
            Type::Str => {
                self.code.push(RegCode::NCall(4, vec![reg]));
            }
            _ => {}
        }
    }

    fn compile_complex_print(&mut self, reg: u8, arg_ty: Type<'src>, recur: usize) {
        if recur > 5 {
            // print ...
            let const_ellipses = self.consts.len();
            self.consts.push((Type::Str, b"...".to_vec()));
            let const_reg = self.get_reg();
            self.code.push(RegCode::Const(const_ellipses, const_reg));
            self.code.push(RegCode::NCall(4, vec![const_reg]));
            self.free_reg(const_reg);
            return;
        }
        match arg_ty {
            Type::List(ty) => {
                // compile into a loop
                // NCall(6, [reg]) is for length
                // ListGet(reg, reg, reg) is for getting the element
                let const_ix = self.consts.len();
                self.consts.push((Type::Int, 0u64.to_le_bytes().to_vec()));
                let const_ix_1 = self.consts.len();
                self.consts.push((Type::Int, 1u64.to_le_bytes().to_vec()));
                let _1_reg = self.get_reg();
                self.code.push(RegCode::Const(const_ix_1, _1_reg));
                // ,
                let comma_reg = self.get_reg();
                let const_comma = self.consts.len();
                self.consts.push((Type::Str, b", ".to_vec()));
                self.code.push(RegCode::Const(const_comma, comma_reg));
                // [
                let open_reg = self.get_reg();
                let const_open = self.consts.len();
                self.consts.push((Type::Char, vec![b'[']));
                self.code.push(RegCode::Const(const_open, open_reg));
                // ]
                let close_reg = self.get_reg();
                let const_close = self.consts.len();
                self.consts.push((Type::Char, vec![b']']));
                self.code.push(RegCode::Const(const_close, close_reg));
                // print [
                self.code.push(RegCode::NCall(2, vec![open_reg]));
                let ix_reg = self.get_reg();
                self.code.push(RegCode::Const(const_ix, ix_reg));
                let len_reg = self.get_reg();
                self.code.push(RegCode::NCall(6, vec![reg]));
                self.code.push(RegCode::Move(len_reg, 0));
                let loop_start = self.code.len();
                let cond_reg = self.get_reg();
                self.code.push(RegCode::ILt(ix_reg, len_reg, cond_reg));
                let jump_ix = self.code.len();
                self.code.push(RegCode::Nop);
                let el_reg = self.get_reg();
                self.code.push(RegCode::ListGet(reg, ix_reg, el_reg));
                self.compile_complex_print(el_reg, *ty, recur + 1);
                // print , if not last
                let is_last_cond_reg = self.get_reg();
                let len_minus_1_reg = self.get_reg();
                self.code
                    .push(RegCode::ISub(len_reg, _1_reg, len_minus_1_reg));
                self.code
                    .push(RegCode::INe(ix_reg, len_minus_1_reg, is_last_cond_reg));
                let jump_ix2 = self.code.len();
                self.code.push(RegCode::Nop);
                self.code.push(RegCode::NCall(4, vec![comma_reg]));
                self.code[jump_ix2] = RegCode::JmpIfNot(is_last_cond_reg, self.code.len());
                self.code.push(RegCode::IAdd(_1_reg, ix_reg, ix_reg));
                self.code.push(RegCode::Jmp(loop_start));
                self.code[jump_ix] = RegCode::JmpIfNot(cond_reg, self.code.len());
                // print ]
                self.code.push(RegCode::NCall(2, vec![close_reg]));
                self.free_reg(ix_reg);
                self.free_reg(len_reg);
                self.free_reg(cond_reg);
                self.free_reg(el_reg);
                self.free_reg(_1_reg);
                self.free_reg(comma_reg);
                self.free_reg(open_reg);
                self.free_reg(close_reg);
                self.free_reg(len_minus_1_reg);
                self.free_reg(is_last_cond_reg);
            }
            Type::Map(_, _) => {
                // cant print maps as of now
            }
            Type::Tuple(tys) => {
                // compile into a loop
                // compile into a loop
                // NCall(6, [reg]) is for length
                // ListGet(reg, reg, reg) is for getting the element
                self.consts.push((Type::Int, 0u64.to_le_bytes().to_vec()));
                let const_ix_1 = self.consts.len();
                self.consts.push((Type::Int, 1u64.to_le_bytes().to_vec()));
                let _1_reg = self.get_reg();
                self.code.push(RegCode::Const(const_ix_1, _1_reg));
                // ,
                let comma_reg = self.get_reg();
                let const_comma = self.consts.len();
                self.consts.push((Type::Str, b", ".to_vec()));
                self.code.push(RegCode::Const(const_comma, comma_reg));
                // (
                let open_reg = self.get_reg();
                let const_open = self.consts.len();
                self.consts.push((Type::Char, vec![b'(']));
                self.code.push(RegCode::Const(const_open, open_reg));
                // )
                let close_reg = self.get_reg();
                let const_close = self.consts.len();
                self.consts.push((Type::Char, vec![b')']));
                self.code.push(RegCode::Const(const_close, close_reg));
                // print (
                self.code.push(RegCode::NCall(2, vec![open_reg]));
                for (i, ty) in tys.iter().enumerate() {
                    // do TupleGet
                    // compile_complex_print
                    let el_reg = self.get_reg();
                    let const_ix = self.consts.len();
                    let const_reg = self.get_reg();
                    self.consts.push((Type::Int, i.to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_ix, const_reg));
                    self.code.push(RegCode::TupleGet(reg, const_reg, el_reg));
                    self.compile_complex_print(el_reg, ty.clone(), recur + 1);
                    self.code.push(RegCode::NCall(4, vec![comma_reg]));
                    self.free_reg(const_reg);
                    self.free_reg(el_reg);
                }
                // print )
                self.code.push(RegCode::NCall(2, vec![close_reg]));
                self.free_reg(_1_reg);
                self.free_reg(comma_reg);
                self.free_reg(open_reg);
                self.free_reg(close_reg);
            }
            Type::Custom(tyname) => {
                let actual_ret_type = self.typemap.get(tyname);
                if actual_ret_type.is_some() {
                    self.compile_complex_print(reg, actual_ret_type.unwrap().clone(), recur + 1);
                } else {
                    panic!("Unknown type: {}", tyname);
                }
            }
            ty => {
                self.compile_print(reg, ty);
            }
        }
    }

    fn emit_stack_map(&self) -> Vec<u64> {
        // emit stack map for GC to know which locals are live
        // convert live_vars to a bitset
        let mut stack_map = vec![];
        let mut bitset = 0u64;
        for i in 0..self.locals.prev_local {
            if i > 0 && i % 64 == 0 {
                stack_map.push(bitset);
                bitset = 0;
            }
            if self.locals.is_live(i) {
                bitset |= 1 << i % 64;
            }
        }
        if bitset > 0 {
            stack_map.push(bitset);
        }
        return stack_map;
    }

    fn compile_expr(&mut self, expr: &Spanned<Expr<'src>>) {
        // println!("compiling expr: {:?}", expr);
        self.span = (expr.0, expr.2);
        match &expr.1 {
            Expr::Literal { lit, ty: _ } => {
                match lit {
                    Lit::Int(i) => {
                        self.consts.push((Type::Int, i.to_le_bytes().to_vec()));
                    }
                    Lit::Double(d) => {
                        let bytes = d.to_bits();
                        self.consts
                            .push((Type::Double, bytes.to_le_bytes().to_vec()));
                    }
                    Lit::Char(c) => {
                        self.consts.push((Type::Char, vec![*c]));
                    }
                    Lit::Bool(b) => {
                        if *b {
                            self.consts.push((Type::Bool, vec![1]));
                        } else {
                            self.consts.push((Type::Bool, vec![0]));
                        }
                    }
                    Lit::Str(s) => {
                        // encode as utf8
                        let mut bytes = vec![];
                        for c in s.chars() {
                            let mut buf = [0u8; 4];
                            let len = c.encode_utf8(&mut buf).len();
                            for i in 0..len {
                                bytes.push(buf[i]);
                            }
                        }
                        self.consts.push((Type::Str, bytes));
                    }
                }
                let const_idx = self.consts.len() - 1;
                let reg = self.get_reg();
                self.code.push(RegCode::Const(const_idx, reg));
                self.stack_push(reg);
            }
            Expr::Variable { name, ty: _ } => {
                let local = self.get_local(&name);
                if local.is_some() {
                    let local = *local.unwrap();
                    let reg = self.get_reg();
                    self.code.push(RegCode::Load(local, reg, 0));
                    // println!("{:?}", self.code);
                    self.stack_push(reg);
                } else {
                    // it is variant
                    let ty = self.variants.get(*name);
                    if ty.is_some() {
                        // find variant ix
                        let variant_ix = ty.unwrap().2;
                        let varreg = self.get_reg();
                        let const_reg = self.get_reg();
                        let const_ix = self.consts.len();
                        self.consts
                            .push((Type::Int, variant_ix.to_le_bytes().to_vec()));
                        self.code.push(RegCode::Const(const_ix, const_reg));
                        self.code.push(RegCode::NCall(8, vec![0, const_reg]));
                        self.free_reg(const_reg);
                        self.code.push(RegCode::Move(varreg, 0));
                        self.stack_push(varreg);
                    } else {
                        let lit = self.constmap.get(*name);
                        if lit.is_some() {
                            let lit = lit.unwrap().1.clone();
                            let ty = get_literal_type(&lit);
                            let literal = Expr::Literal { lit, ty };
                            self.compile_expr(&(expr.0, literal, expr.2));
                        } else {
                            panic!("Unknown variable: {}", name);
                        }
                    }
                }
            }
            Expr::Arith {
                op,
                left,
                right,
                ty,
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_unfree_pop();
                let arg1 = self.stack_unfree_pop();
                let op = if ty.clone().unwrap() == Type::Int {
                    match op {
                        ArithOp::Add => RegCode::IAdd,
                        ArithOp::Sub => RegCode::ISub,
                        ArithOp::Mul => RegCode::IMul,
                        ArithOp::Div => RegCode::IDiv,
                        ArithOp::Mod => RegCode::IRem,
                    }
                } else if ty.clone().unwrap() == Type::Double {
                    match op {
                        ArithOp::Add => RegCode::FAdd,
                        ArithOp::Sub => RegCode::FSub,
                        ArithOp::Mul => RegCode::FMul,
                        ArithOp::Div => RegCode::FDiv,
                        ArithOp::Mod => RegCode::FRem,
                    }
                } else {
                    panic!("Unknown type: {:?}", ty);
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
                self.free_reg(arg1);
                self.free_reg(arg2);
            }
            Expr::BoolOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_unfree_pop();
                let arg1 = self.stack_unfree_pop();
                let op = match op {
                    BoolOp::And => RegCode::LAnd,
                    BoolOp::Or => RegCode::LOr,
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
                self.free_reg(arg1);
                self.free_reg(arg2);
            }
            Expr::BitOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_unfree_pop();
                let arg1 = self.stack_unfree_pop();
                let op = match op {
                    BitOp::And => RegCode::BAnd,
                    BitOp::Or => RegCode::BOr,
                    BitOp::Xor => RegCode::BXor,
                    BitOp::Shl => RegCode::BShl,
                    BitOp::Shr => RegCode::BShr,
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
                self.free_reg(arg1);
                self.free_reg(arg2);
            }
            Expr::CmpOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_unfree_pop();
                let arg1 = self.stack_unfree_pop();
                let ty = left.1.get_type();
                let ty2 = right.1.get_type();
                let op = if ty == Type::Int || ty == Type::Bool || ty == Type::Char {
                    if ty == Type::Bool || ty == Type::Char {
                        // convert to int
                        self.code.push(RegCode::Cast(arg1, Type::Int, ty, arg1));
                        self.code.push(RegCode::Cast(arg2, Type::Int, ty2, arg2));
                    }
                    match op {
                        CmpOp::Eq => RegCode::IEq,
                        CmpOp::Ne => RegCode::INe,
                        CmpOp::Lt => RegCode::ILt,
                        CmpOp::Gt => RegCode::IGt,
                        CmpOp::Le => RegCode::ILe,
                        CmpOp::Ge => RegCode::IGe,
                    }
                } else {
                    match op {
                        CmpOp::Eq => RegCode::FEq,
                        CmpOp::Ne => RegCode::FNe,
                        CmpOp::Lt => RegCode::FLt,
                        CmpOp::Gt => RegCode::FGt,
                        CmpOp::Le => RegCode::FLe,
                        CmpOp::Ge => RegCode::FGe,
                    }
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
                self.free_reg(arg1);
                self.free_reg(arg2);
            }
            Expr::Neg { expr, ty } => {
                self.compile_expr(&expr);
                let arg = self.stack_unfree_pop();
                let op = if ty.clone().unwrap() == Type::Int {
                    RegCode::INeg
                } else {
                    RegCode::FNeg
                };
                let reg = self.get_reg();
                self.code.push(op(arg, reg));
                self.stack_push(reg);
                self.free_reg(arg);
            }
            Expr::Not { expr, .. } => {
                self.compile_expr(&expr);
                let arg = self.stack_unfree_pop();
                let reg = self.get_reg();
                self.code.push(RegCode::LNot(arg, reg));
                self.stack_push(reg);
                self.free_reg(arg);
            }
            Expr::BitNot { expr, .. } => {
                self.compile_expr(&expr);
                let arg = self.stack_unfree_pop();
                let reg = self.get_reg();
                self.code.push(RegCode::BNot(arg, reg));
                self.stack_push(reg);
                self.free_reg(arg);
            }
            Expr::Call { name, args, ty } => {
                for arg in args {
                    self.compile_expr(&arg);
                }
                let mut arg_regs = vec![];
                for _ in 0..args.len() {
                    arg_regs.push(self.stack.pop().unwrap());
                }
                let arg_regs = arg_regs.into_iter().rev().collect::<Vec<_>>();
                let name = match *name.clone() {
                    Expr::Variable { name, .. } => Some(name),
                    _ => None,
                };
                if let Some(name) = name {
                    if name == "print" {
                        let argtys = args.iter().map(|e| e.1.get_type());
                        // self.code.push(RegCode::PrintLock);
                        for arg in arg_regs.iter().zip(argtys) {
                            self.compile_complex_print(*arg.0, arg.1, 0);
                        }
                        // self.code.push(RegCode::PrintUnlock);
                        return;
                    } else if let Some((native_ix, _arity, returns)) = self.builtin(&name) {
                        self.code.push(RegCode::NCall(native_ix, arg_regs));
                        if returns {
                            let reg = self.get_reg();
                            self.code.push(RegCode::Move(reg, 0));
                            self.stack_push(reg);
                        }
                        return;
                    }
                    let reg = self.get_reg();

                    if let Some(ty) = self.variants.get(name) {
                        // check which variant it is
                        let variant_ix = ty.2;
                        // NCall(8, [reg, variant_ix])
                        let varreg = self.get_reg();
                        let variant_ix_reg = self.get_reg();
                        let const_variant_ix = self.consts.len();
                        self.consts
                            .push((Type::Int, variant_ix.to_le_bytes().to_vec()));
                        self.code
                            .push(RegCode::Const(const_variant_ix, variant_ix_reg));
                        self.code
                            .push(RegCode::NCall(8, vec![arg_regs[0], variant_ix_reg]));
                        self.code.push(RegCode::Move(varreg, 0));
                        self.free_reg(variant_ix_reg);
                        self.stack_push(varreg);
                        return;
                    }

                    let func = self.func_idx.get(name);
                    if func.is_some() {
                        let func = *func.unwrap();
                        // save registers
                        for i in self.stack.iter().rev() {
                            self.code.push(RegCode::Push(*i));
                        }
                        if self.coro_call {
                            self.code.push(RegCode::CoroCall(func, arg_regs));
                        } else {
                            self.code.push(RegCode::Call(func, arg_regs));
                        }
                        // restore registers
                        for i in self.stack.iter() {
                            self.code.push(RegCode::Pop(*i));
                        }
                        if ty.is_some() {
                            self.code.push(RegCode::Move(reg, 0));
                            self.stack_push(reg);
                        } else {
                            self.free_reg(reg);
                        }
                    } else {
                        panic!("Unknown function: {}", name);
                    }
                } else {
                    unimplemented!("Cannot compile call using a complex expression");
                }
            }
            Expr::Subscr { expr, index, .. } => {
                self.compile_expr(&expr);
                self.compile_expr(&index);
                let idx = self.stack_pop();
                let ex = self.stack_pop();
                let op = match expr.1.get_type() {
                    Type::List(_) => RegCode::ListGet,
                    Type::Map(_, _) => RegCode::MapGet,
                    Type::Str => RegCode::StrGet,
                    Type::Tuple(_) => RegCode::TupleGet,
                    Type::Variant(_) => {
                        // NCall(10, [ex])
                        self.code.push(RegCode::NCall(10, vec![ex]));
                        let reg = self.get_reg();
                        self.code.push(RegCode::Move(reg, 0));
                        self.stack_push(reg);
                        self.free_reg(ex);
                        self.free_reg(idx);
                        return;
                    }
                    _ => unreachable!("Unknown type: {:?}", expr.1.get_type()),
                };
                let reg = self.get_reg();
                self.code.push(op(ex, idx, reg));
                self.stack_push(reg);
            }
            Expr::Assign { left, right } => {
                self.compile_expr(&right);
                let arg = self.stack_unfree_pop();
                match &(*left).1 {
                    Expr::Variable { name, .. } => {
                        let var = self.get_local(name);
                        match var {
                            Some(var) => {
                                let v = *var;
                                self.code.push(RegCode::Store(v, arg, 0));
                                self.locals.set_live_var(v);
                            }
                            None => {
                                unreachable!("Unknown variable: {}", name);
                            }
                        }
                    }
                    Expr::Subscr { expr, index, .. } => {
                        self.compile_expr(expr);
                        let ex_arg = self.stack_unfree_pop();
                        self.compile_expr(index);
                        let ix_arg = self.stack_unfree_pop();
                        let op = match (*expr).1.get_type() {
                            Type::List(_) => RegCode::ListSet,
                            Type::Map(_, _) => RegCode::MapSet,
                            Type::Tuple(_) => RegCode::TupleSet,
                            _ => unreachable!(),
                        };
                        self.code.push(op(ex_arg, ix_arg, arg));
                        self.free_reg(ix_arg);
                        self.free_reg(ex_arg);
                    }
                    _ => unreachable!(),
                }
                self.free_reg(arg);
            }
            Expr::Is { expr, ix, .. } => {
                self.compile_expr(&expr);
                let arg = self.stack_unfree_pop();
                let reg = self.get_reg();
                let is_res = self.get_reg();
                let const_ix = self.consts.len();
                self.consts
                    .push((Type::Int, (*ix).unwrap().to_le_bytes().to_vec()));
                self.code.push(RegCode::Const(const_ix, reg));
                self.code.push(RegCode::NCall(9, vec![arg, reg]));
                self.code.push(RegCode::Move(is_res, 0));
                self.free_reg(arg);
                self.free_reg(reg);
                self.stack_push(is_res);
            }
            Expr::Make { ty, expr: size } => {
                let reg = self.get_reg();
                let size = match size {
                    Some(size) => {
                        self.compile_expr(&size);
                        let size = self.stack_unfree_pop();
                        size
                    }
                    None => {
                        let const_0_reg = self.get_reg();
                        let const_0 = self.consts.len();
                        self.consts.push((Type::Int, 0u64.to_le_bytes().to_vec()));
                        self.code.push(RegCode::Const(const_0, const_0_reg));
                        const_0_reg
                    }
                };
                self.code.push(RegCode::Make(ty.clone(), reg, size));
                self.stack_push(reg);
                self.free_reg(size);
            }
            Expr::Cast { expr, ty } => {
                self.compile_expr(&expr);
                let arg = self.stack_pop();
                let reg = self.get_reg();
                self.code
                    .push(RegCode::Cast(arg, expr.1.get_type(), ty.clone(), reg));
                self.stack_push(reg);
            }
            Expr::Tuple { exprs, .. } => {
                let reg = self.get_reg();
                let tys = exprs.iter().map(|e| e.1.get_type()).collect::<Vec<_>>();
                self.code.push(RegCode::Tuple(exprs.len(), reg, tys));
                for expr in exprs.iter().enumerate().rev() {
                    self.compile_expr(&expr.1);
                    let const_reg = self.get_reg();
                    let val_reg = self.stack_pop();
                    let const_ix = self.consts.len();
                    self.consts.push((Type::Int, expr.0.to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_ix, const_reg));
                    self.code.push(RegCode::TupleSet(reg, const_reg, val_reg));
                    self.free_reg(const_reg);
                    self.free_reg(val_reg);
                }
                self.stack_push(reg);
            }
            Expr::List { exprs, .. } => {
                let reg = self.get_reg();
                let len_reg = self.get_reg();
                let const_ix = self.consts.len();
                self.consts
                    .push((Type::Int, exprs.len().to_le_bytes().to_vec()));
                self.code.push(RegCode::Const(const_ix, len_reg));
                for expr in exprs.iter().rev() {
                    self.compile_expr(&expr);
                }
                self.code.push(RegCode::Make(
                    Type::List(Box::new(exprs[0].1.get_type())),
                    reg,
                    len_reg,
                ));
                for ix in 0..exprs.len() {
                    let vreg = self.stack_pop();
                    let const_ix = self.consts.len();
                    self.consts.push((Type::Int, ix.to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_ix, len_reg));
                    self.code.push(RegCode::ListSet(reg, len_reg, vreg));
                }
                self.free_reg(len_reg);
                self.stack_push(reg);
            }
            Expr::ChanRead { name, ty } => {
                let reg = self.get_reg();
                let cloned = self.get_reg();
                let chan = self.locals.get(name).unwrap();
                self.code.push(RegCode::ChanRecv(*chan, reg));
                if ty.clone().unwrap().is_heap_type() {
                    self.code.push(RegCode::Clone(reg, cloned, true));
                    self.stack_push(cloned);
                    self.free_reg(reg);
                } else {
                    self.stack_push(reg);
                    self.free_reg(cloned);
                }
            }
            _ => {
                unimplemented!();
            }
        }
        // println!("stack: {:?}", self.stack);
    }

    fn compile_stmt(&mut self, stmt: &'a Spanned<Stmt<'src>>) {
        match &(*stmt).1 {
            Stmt::Expr(expr) => {
                self.compile_expr(&expr);
                self.stack.clear();
                self.code.push(RegCode::FreeRegs);
            }
            Stmt::IfElse(cond, then, els) => {
                self.compile_expr(&cond);
                let arg = self.stack_pop();
                self.free_regs[arg as usize] = false; // no other instr should use it
                let jmp_ix = self.code.len();
                self.code.push(RegCode::Nop);
                self.locals.push();
                for stmt in then {
                    self.compile_stmt(&stmt);
                }
                self.locals.pop();
                let jmp_ix2 = self.code.len();
                self.code.push(RegCode::Nop);
                self.code[jmp_ix] = RegCode::JmpIfNot(arg, jmp_ix2 + 1);
                if let Some(else_body) = els {
                    self.locals.push();
                    for stmt in else_body {
                        self.compile_stmt(&stmt);
                    }
                    self.locals.pop();
                }
                let jmp_ix3 = self.code.len();
                self.code[jmp_ix2] = RegCode::Jmp(jmp_ix3);
                self.free_reg(arg);
            }
            Stmt::Decl(var, expr) => {
                self.compile_expr(&expr);
                match &(*var).1 {
                    Expr::Variable { name, ty } => {
                        let lcl = self.add_local(&name);
                        let reg = self.stack_pop();
                        self.code.push(RegCode::Store(lcl, reg, 0));
                        if let Some(ty) = ty {
                            if ty.is_heap_type() {
                                self.locals.set_live_var(lcl);
                                let stmap = self.emit_stack_map();
                                self.code.push(RegCode::StackMap(stmap));
                            }
                        }
                        self.free_reg(reg);
                    }
                    Expr::Tuple { exprs, .. } => {
                        let tuple_val = self.stack_unfree_pop();
                        for expr in exprs.iter().enumerate() {
                            let const_reg = self.get_reg();
                            let const_ix = self.consts.len();
                            self.consts.push((Type::Int, expr.0.to_le_bytes().to_vec()));
                            self.code.push(RegCode::Const(const_ix, const_reg));
                            match &expr.1 .1 {
                                Expr::Variable { name, ty } => {
                                    let lcl = self.add_local(&name);
                                    if let Some(ty) = ty {
                                        if ty.is_heap_type() {
                                            self.locals.set_live_var(lcl);
                                        }
                                    }
                                    let reg = self.get_reg();
                                    self.code.push(RegCode::TupleGet(tuple_val, const_reg, reg));
                                    self.code.push(RegCode::Store(lcl, reg, 0));
                                    self.free_reg(reg);
                                }
                                _ => {
                                    unimplemented!("Cannot destructure {:?}", expr.1);
                                }
                            }
                        }
                        self.free_reg(tuple_val);
                    }
                    _ => {
                        println!("cannot destructure {:?}", var);
                        exit(1);
                    }
                }
            }
            Stmt::For(var, expr, body) => {
                // if expr is Range then set var to range.start and loop until range.end
                // ele introduce new variable and loop until expr.len
                let var_ix = self.locals.insert(var);
                self.breaks.push(vec![]);
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                } = &(*expr).1
                {
                    self.compile_expr(&start);
                    let start = self.stack_unfree_pop();
                    self.code.push(RegCode::Store(var_ix, start, 0));
                    self.compile_expr(&end);
                    let end = self.stack_unfree_pop();
                    let cond_reg = self.get_reg();
                    let iter = self.get_reg();
                    let const_reg = self.get_reg();
                    let const_ix_1 = self.consts.len();
                    // push iter and end to abstract stack so that if there is a function call
                    // inside the loop, it doesnt get overwritten
                    self.stack.push(iter);
                    self.stack.push(end);
                    self.locals.push();
                    self.consts.push((Type::Int, 1u64.to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_ix_1, const_reg));
                    self.code.push(RegCode::Store(var_ix, start, 0));
                    self.code.push(RegCode::Load(var_ix, iter, 0));
                    let jmp_over_icr = self.code.len();
                    self.code.push(RegCode::Nop);
                    let incr_jmp_ix = self.code.len();
                    self.code.push(RegCode::IAdd(iter, const_reg, iter));
                    self.code.push(RegCode::Store(var_ix, iter, 0));
                    self.code.push(RegCode::Load(var_ix, iter, 0));
                    self.code[jmp_over_icr] = RegCode::Jmp(self.code.len());
                    if *inclusive {
                        self.code.push(RegCode::ILe(iter, end, cond_reg));
                    } else {
                        self.code.push(RegCode::ILt(iter, end, cond_reg));
                    };
                    let jmp_ix2 = self.code.len();
                    self.code.push(RegCode::Nop);
                    self.loop_starts.push(incr_jmp_ix);
                    for stmt in body {
                        self.compile_stmt(&stmt);
                    }
                    self.code.push(RegCode::Jmp(incr_jmp_ix));
                    let jmp_ix4 = self.code.len();
                    self.code[jmp_ix2] = RegCode::JmpIfNot(cond_reg, jmp_ix4);
                    // pop iter from abstract stack
                    self.stack.pop(); // end
                    self.stack.pop(); // iter
                    self.free_reg(iter);
                    self.free_reg(cond_reg);
                    self.free_reg(const_reg);
                    let loop_ix: usize = self.breaks.len() - 1;
                    for ix in self.breaks[loop_ix].iter() {
                        self.code[*ix] = RegCode::Jmp(jmp_ix4);
                    }
                    self.locals.pop();
                    self.loop_starts.pop();
                } else {
                    self.compile_expr(&expr);
                    let arg = self.stack_unfree_pop();
                    let iter = self.get_reg();
                    let cond_reg = self.get_reg();
                    // assign var
                    let reg = self.get_reg();
                    let len_reg = self.get_reg();
                    self.locals.push();
                    self.code.push(RegCode::NCall(6, vec![arg]));
                    self.code.push(RegCode::Move(len_reg, 0));
                    let const_neg_ix = self.consts.len();
                    self.consts
                        .push((Type::Int, (-1 as i64).to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_neg_ix, iter));
                    let const_one_ix = self.consts.len();
                    let const_one_reg = self.get_reg();
                    self.consts
                        .push((Type::Int, (1 as i64).to_le_bytes().to_vec()));
                    self.code.push(RegCode::Const(const_one_ix, const_one_reg));
                    let jmp_ix = self.code.len();
                    self.code.push(RegCode::IAdd(iter, const_one_reg, iter));
                    self.code.push(RegCode::ILt(iter, len_reg, cond_reg));
                    let jmp_ix2 = self.code.len();
                    self.code.push(RegCode::Nop);
                    self.code.push(RegCode::ListGet(arg, iter, reg));
                    self.code.push(RegCode::Store(var_ix, reg, 0));
                    self.loop_starts.push(jmp_ix);
                    for stmt in body {
                        self.compile_stmt(&stmt);
                    }
                    self.code.push(RegCode::Jmp(jmp_ix));
                    let jmp_ix3 = self.code.len();
                    self.code[jmp_ix2] = RegCode::JmpIfNot(cond_reg, jmp_ix3);
                    let loop_ix: usize = self.breaks.len() - 1;
                    for ix in self.breaks[loop_ix].iter() {
                        self.code[*ix] = RegCode::Jmp(jmp_ix3);
                    }
                    self.locals.pop();
                    self.loop_starts.pop();
                    // free regs
                    self.free_reg(iter);
                    self.free_reg(reg);
                    self.free_reg(len_reg);
                    self.free_reg(cond_reg);
                    self.free_reg(arg);
                }
                self.breaks.pop();
            }
            Stmt::While(cond, body) => {
                let jmp_ix = self.code.len();
                self.compile_expr(&cond);
                self.breaks.push(vec![]);
                let arg = self.stack_unfree_pop();
                let jmp_ix2 = self.code.len();
                self.code.push(RegCode::Nop);
                self.loop_starts.push(jmp_ix);
                self.locals.push();
                for stmt in body {
                    self.compile_stmt(&stmt);
                }
                self.code.push(RegCode::Jmp(jmp_ix));
                let jmp_ix4 = self.code.len();
                self.code[jmp_ix2] = RegCode::JmpIfNot(arg, jmp_ix4);
                self.loop_starts.pop();
                let loop_ix: usize = self.breaks.len() - 1;
                for ix in self.breaks[loop_ix].iter() {
                    self.code[*ix] = RegCode::Jmp(jmp_ix4);
                }
                self.locals.pop();
                self.breaks.pop();

                self.free_reg(arg);
            }
            Stmt::Return(expr) => {
                let ex = *expr.clone();
                if let Some(ex) = ex {
                    self.compile_expr(&ex);
                    let arg = self.stack_pop();
                    self.code.push(RegCode::Return(arg));
                } else {
                    self.code.push(RegCode::VoidReturn);
                }
            }
            Stmt::Break => {
                let jmp_ix = self.code.len();
                self.code.push(RegCode::Nop);
                let loop_ix: usize = self.breaks.len() - 1;
                self.breaks[loop_ix].push(jmp_ix);
            }
            Stmt::Continue => {
                if self.loop_starts.len() == 0 {
                    panic!("Cannot continue outside of loop");
                }
                let start_ix = self.loop_starts.last().unwrap();
                self.code.push(RegCode::Jmp(*start_ix));
            }
            Stmt::ChanWrite(var, expr) => {
                self.compile_expr(&expr);
                let mut arg = self.stack_unfree_pop();
                // if expr is heap type then store then clone it
                if (*expr).1.get_type().is_heap_type() {
                    let reg = self.get_reg();
                    self.code.push(RegCode::Clone(arg, reg, false));
                    self.free_reg(arg);
                    arg = reg;
                }
                let v = self.get_local(&var);
                match v {
                    Some(var) => {
                        self.code.push(RegCode::ChanSend(*var, arg));
                    }
                    None => {
                        unreachable!("Unknown variable: {}", var);
                    }
                }
            }
            Stmt::Coroutine(expr) => {
                self.coro_call = true;
                self.compile_expr(expr);
                self.coro_call = false;
            }
            Stmt::Block(stmts) => {
                self.locals.push();
                for stmt in stmts {
                    self.compile_stmt(stmt);
                }
                self.locals.pop();
            }
        }
    }

    fn parse_super_inst(&mut self) {
        let mut i = 0;
        let mut state = SuperInstParseState::None;
        while i < self.code.len() {
            // println!("{} state: {:?}", i, state);
            match state.clone() {
                SuperInstParseState::None => {
                    if let RegCode::Load(var_ix, reg_ix, _) = self.code[i] {
                        state = SuperInstParseState::Load(var_ix, reg_ix);
                    } else if is_cond_op(&self.code[i]) {
                        state = SuperInstParseState::Cond(self.code[i].clone());
                    } else {
                        state = SuperInstParseState::None;
                    }
                }
                SuperInstParseState::Load(var_ix, reg_ix) => {
                    if let RegCode::Const(const_ix, reg_ix2) = self.code[i] {
                        state = SuperInstParseState::LoadConst(var_ix, const_ix, reg_ix, reg_ix2);
                    } else {
                        state = SuperInstParseState::None;
                    }
                }
                SuperInstParseState::LoadConst(var_ix, const_ix, reg_ix, reg_ix2) => {
                    match three_operand_code(&self.code[i]) {
                        (0, 0, 0) => {
                            state = SuperInstParseState::None;
                        }
                        (r1, r2, res) => {
                            if r1 == reg_ix && r2 == reg_ix2 {
                                state = SuperInstParseState::LoadConstOp(
                                    var_ix,
                                    const_ix,
                                    res,
                                    self.code[i].clone(),
                                );
                            } else {
                                state = SuperInstParseState::None;
                            }
                        }
                    }
                }
                SuperInstParseState::LoadConstOp(var_ix, const_ix, r, op) => {
                    let mut insert_load_const_op = false;
                    if let RegCode::Store(var_ix2, reg_ix2, _) = self.code[i] {
                        if var_ix == var_ix2 && r == reg_ix2 {
                            // remove the load, const, op, store
                            self.code[i - 3] = RegCode::Nop;
                            self.code[i - 2] = RegCode::Nop;
                            self.code[i - 1] = RegCode::Nop;
                            // insert a Super(LoadConstOpStore)
                            self.code[i] = RegCode::Super(SuperInstruction::LoadConstOpStore(
                                var_ix,
                                const_ix,
                                Box::new(op.clone()),
                            ));
                            state = SuperInstParseState::None;
                        } else {
                            insert_load_const_op = true;
                        }
                    } else {
                        insert_load_const_op = true;
                    }

                    if insert_load_const_op {
                        self.code[i - 3] = RegCode::Nop; // op
                        self.code[i - 2] = RegCode::Nop; // const
                        self.code[i - 1] = RegCode::Super(SuperInstruction::LoadConstOp(
                            var_ix,
                            const_ix,
                            r,
                            Box::new(op.clone()),
                        ));
                        i -= 1;
                        state = SuperInstParseState::None;
                    }
                }
                SuperInstParseState::Cond(regcode) => {
                    if let RegCode::JmpIfNot(reg_ix, jmp_ix) = self.code[i] {
                        match three_operand_code(&regcode) {
                            (0, 0, 0) => {}
                            (r1, r2, res) => {
                                if res == reg_ix {
                                    self.code[i - 1] = RegCode::Nop;
                                    self.code[i] = RegCode::Super(SuperInstruction::JmpIfNotCond(
                                        jmp_ix,
                                        r1,
                                        r2,
                                        res,
                                        Box::new(regcode.clone()),
                                    ));
                                }
                            }
                        }
                    }
                    state = SuperInstParseState::None;
                }
            }
            i += 1;
        }
    }

    // fn optimise(&self) {
    //     let mut cfg = construct_cfg(&self.code);
    //     // println!("CFG:");
    //     // for (i, node) in cfg.iter().enumerate() {
    //     //     println!("\t{}: {:?}", i, node);
    //     // }
    //     let succ_nodes = construct_succs_nodes(&cfg, cfg.len());
    //     let rev_dom_tree = construct_revdom_tree(&cfg, &succ_nodes);
    //     // println!("Rev Dominator Tree:");
    //     // for (idx, dom) in rev_dom_tree.iter().enumerate() {
    //     //     println!("\t{}: {:?}", idx, dom);
    //     // }
    //     let idoms = construct_idoms(&rev_dom_tree);
    //     let dom_tree = construct_dom_tree(&idoms, &succ_nodes);
    //     // println!("Dominator Tree:");
    //     // for (idx, dom) in dom_tree.iter().enumerate() {
    //     //     println!("\t{}: {:?}", idx, dom);
    //     // }
    //     // println!("idoms: ");
    //     // for (i, j) in idoms.clone().into_iter().enumerate() {
    //     //     println!("{} {}", i, j);
    //     // }
    //     // println!("Dominance Frontiers:");
    //     let domf = construct_dominance_frontiers(&succ_nodes, &dom_tree, &idoms);
    //     // for (idx, dom) in domf.iter().enumerate() {
    //     //     println!("\t{}: {:?}", idx, dom);
    //     // }
    //     // println!("Phis inserted and variables renamed");
    //     insert_phi_functions(&mut cfg, &domf, self.locals.prev_local);
    //     let mut vmap = HashMap::new();
    //     let mut visited = vec![false; cfg.len()];
    //     for idx in 0..cfg.len() {
    //         rename_variable(idx, &mut cfg, &dom_tree, &mut vmap, &mut visited)
    //     }
    //     // for (idx, node) in cfg.iter().enumerate() {
    //     //     println!("\t{}: {:?}", idx, node);
    //     // }
    // }

    fn compile_func(&mut self, func: &'a Func<'src>) {
        for arg in &func.args {
            let lcl = self.locals.insert(arg.name);
            if arg.ty.is_heap_type() {
                self.locals.set_live_var(lcl);
            }
        }
        let stmap = self.emit_stack_map();
        self.code.push(RegCode::StackMap(stmap));
        self.curr_func = self.func_idx[&func.name];
        for stmt in &func.body {
            self.compile_stmt(&stmt);
        }
        self.code.push(RegCode::Return(0));
    }

    pub fn compile_program(&mut self, prog: &'a Program<'src>, super_inst: bool) {
        let fns = &prog
            .top_levels
            .iter()
            .filter(|tl| match tl {
                TopLevel::Func(_) => true,
                _ => false,
            })
            .map(|tl| match tl {
                TopLevel::Func(f) => f,
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        let mut idx = 0;
        for func in fns {
            self.func_idx.insert(func.name, idx);
            idx += 1;
        }
        idx = 0;
        let mut func_code = Vec::new();
        for func in fns {
            self.curr_func = idx;
            self.locals.clear();
            let args = &func.args.iter().map(|arg| arg.name).collect::<Vec<_>>();
            self.func_args_len.push(args.len() as u32);
            if func.name == "main" {
                self.start_func_idx = idx;
            }
            self.compile_func(func);
            // self.optimise();
            if super_inst {
                self.parse_super_inst();
            }
            func_code.push(self.code.clone());
            self.code.clear();
            idx += 1;
        }
        self.func_code = func_code;
        self.locals.clear();
    }
}

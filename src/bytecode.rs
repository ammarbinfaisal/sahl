use crate::syntax::*;
use std::collections::HashMap;
use std::fs::*;
use std::io::Write;

const IADD: u8 = 0;
const ISUB: u8 = 1;
const IMUL: u8 = 2;
const IDIV: u8 = 3;
const IMOD: u8 = 4;
const INEG: u8 = 5;
const NOT: u8 = 6;
const AND: u8 = 7;
const OR: u8 = 8;
const EQUAL: u8 = 9;
const NOT_EQUAL: u8 = 10;
const ILESS: u8 = 11;
const ILESS_EQUAL: u8 = 12;
const IGREATER: u8 = 13;
const IGREATER_EQUAL: u8 = 14;
const TRUE: u8 = 15;
const FALSE: u8 = 16;
const JUMP: u8 = 17;
const JUMP_IF_FALSE: u8 = 18;
const STORE: u8 = 19;
const LIST_INDEX: u8 = 20;
const LIST_APPEND: u8 = 21;
const LIST_LENGTH: u8 = 22;
const LIST: u8 = 23;
const CONST_U64: u8 = 24;
// const CONST_U32: u8 = 25;
const CONST_U8: u8 = 26;
const STRING: u8 = 27;
const DEF_LOCAL: u8 = 28;
const GET_LOCAL: u8 = 29;
const ASSIGN: u8 = 30;
const CALL: u8 = 31;
const RETURN: u8 = 32;
// const PRINT: u8 = 33;
// const POP: u8 = 34;
const MAKE_LIST: u8 = 35;
const MAKE_TUPLE: u8 = 36;
const NATIVE_CALL: u8 = 37;
const CONST_DOUBLE: u8 = 38;
const MAKE_CHAN: u8 = 39;
const CHAN_READ: u8 = 40;
const CHAN_WRITE: u8 = 41;
const SPAWN: u8 = 42;
const MAKE_MAP: u8 = 43;
const FADD: u8 = 44;
const FSUB: u8 = 45;
const FMUL: u8 = 46;
const FDIV: u8 = 47;
const FNEG: u8 = 48;
const FLESS: u8 = 49;
const FLESS_EQUAL: u8 = 50;
const FGREATER: u8 = 51;
const FGREATER_EQUAL: u8 = 52;
const SCONCAT: u8 = 53;
// const I2F: u8 = 54; use CAST instead
const I2S: u8 = 55;
const F2S: u8 = 56;
const FMOD: u8 = 57;
const BIT_AND: u8 = 58;
const BIT_OR: u8 = 59;
const BIT_XOR: u8 = 60;
const BIT_SHL: u8 = 61;
const BIT_SHR: u8 = 62;
const CAST: u8 = 63; // <from> <to>

const MAX_LOCALS: usize = (i64::pow(2, 32) - 1) as usize;

#[derive(Debug, Clone, PartialEq)]
pub enum LoopType {
    While,
    For,
    Range,
    None,
}

#[derive(Debug, Clone)]
pub struct LoopState {
    pub loop_type: LoopType,
    pub loop_start: Option<usize>,
    pub loop_end_var: Option<usize>,
    pub loop_forw_var: Option<usize>,
    pub idx_var: Option<usize>,
    pub breaks: Vec<usize>,
}

impl LoopState {
    pub fn new() -> LoopState {
        LoopState {
            loop_type: LoopType::None,
            loop_start: None,
            loop_end_var: None,
            loop_forw_var: None,
            idx_var: None,
            breaks: Vec::new(),
        }
    }
}

pub struct Bytecode {
    code: Vec<u8>,
    locals: HashMap<String, usize>,
    func_idx: HashMap<String, usize>,
    calls: Vec<Vec<(usize, usize)>>, // to be patched after codegen - offset, func_idx of call site
    strings: Vec<Vec<u8>>,
    func_code: Vec<Vec<u8>>,
    func_args_len: Vec<u32>,
    loop_state: LoopState,
    start_func_idx: usize,
    curr_func: usize,
    opcode_span: HashMap<usize, (usize, usize)>,
    span: (usize, usize),
    source_name: String,
}

fn u32_to_bytes(n: u32) -> [u8; 4] {
    [n as u8, (n >> 8) as u8, (n >> 16) as u8, (n >> 24) as u8]
}

fn ty_to_int(ty: Type) -> u32 {
    // type
    // INT - 1
    // FLOAT - 2
    // BOOL - 3
    // Char - 4
    match ty {
        Type::Int => 1,
        Type::Double => 2,
        Type::Bool => 3,
        Type::Char => 4,
        _ => 0,
    }
}

impl Bytecode {
    pub fn new(filename: String) -> Bytecode {
        Bytecode {
            code: Vec::new(),
            locals: HashMap::new(),
            func_idx: HashMap::new(),
            calls: Vec::new(),
            strings: Vec::new(),
            loop_state: LoopState::new(),
            func_code: Vec::new(),
            func_args_len: Vec::new(),
            start_func_idx: 0,
            curr_func: 0,
            opcode_span: HashMap::new(),
            span: (0, 0),
            source_name: filename,
        }
    }

    fn add(&mut self, op: u8) {
        self.opcode_span.insert(self.code.len(), self.span);
        self.code.push(op);
    }

    fn add_u8(&mut self, op: u8, arg: u8) -> usize {
        self.opcode_span.insert(self.code.len(), self.span);
        self.code.push(op);
        self.code.push(arg);
        self.code.len() - 1
    }

    fn add_u32(&mut self, op: u8, n: u32) -> usize {
        self.opcode_span.insert(self.code.len(), self.span);
        self.code.push(op);
        self.code.push(n as u8);
        self.code.push((n >> 8) as u8);
        self.code.push((n >> 16) as u8);
        self.code.push((n >> 24) as u8);
        self.code.len() - 4
    }

    fn add_u64(&mut self, op: u8, n: u64) -> usize {
        self.opcode_span.insert(self.code.len(), self.span);
        self.code.push(op);
        self.code.push(n as u8);
        self.code.push((n >> 8) as u8);
        self.code.push((n >> 16) as u8);
        self.code.push((n >> 24) as u8);
        self.code.push((n >> 32) as u8);
        self.code.push((n >> 40) as u8);
        self.code.push((n >> 48) as u8);
        self.code.push((n >> 56) as u8);
        self.code.len() - 8
    }

    fn add_2_u32(&mut self, op: u8, n1: u32, n2: u32) -> (usize, usize) {
        self.opcode_span.insert(self.code.len(), self.span);
        self.code.push(op);
        self.code.push(n1 as u8);
        self.code.push((n1 >> 8) as u8);
        self.code.push((n1 >> 16) as u8);
        self.code.push((n1 >> 24) as u8);
        self.code.push(n2 as u8);
        self.code.push((n2 >> 8) as u8);
        self.code.push((n2 >> 16) as u8);
        self.code.push((n2 >> 24) as u8);
        (self.code.len() - 8, self.code.len() - 4)
    }

    fn patch_u32(&mut self, ip: usize, n: u32) {
        self.code[ip + 3] = (n >> 24) as u8;
        self.code[ip + 2] = (n >> 16) as u8;
        self.code[ip + 1] = (n >> 8) as u8;
        self.code[ip] = n as u8;
    }

    fn add_local(&mut self, name: &str) -> usize {
        let n = self.locals.len();
        if n > MAX_LOCALS {
            panic!("too many locals");
        }
        self.locals.insert(name.to_string(), n);
        n
    }

    fn get_local(&self, name: &str) -> Option<&usize> {
        self.locals.get(name)
    }

    // start ip
    fn header(&self) -> [u8; 4] {
        u32_to_bytes(self.start_func_idx as u32)
    }

    pub fn write(&self, path: &str) {
        let mut file = File::create(path).unwrap();
        // filename length
        file.write_all(&u32_to_bytes(self.source_name.len() as u32))
            .unwrap();
        // filename
        file.write_all(self.source_name.as_bytes()).unwrap();
        file.write_all(&self.header()).unwrap();
        // u32 for number of strings
        file.write_all(&u32_to_bytes(self.strings.len() as u32))
            .unwrap();
        for s in &self.strings {
            file.write_all(&u32_to_bytes(s.len() as u32)).unwrap();
            file.write_all(&s).unwrap();
        }
        file.write_all(&u32_to_bytes(self.func_code.len() as u32))
            .unwrap();
        let mut i = 0;
        for f in &self.func_code {
            file.write_all(&u32_to_bytes(f.len() as u32)).unwrap();
            file.write_all(&u32_to_bytes(self.func_args_len[i] as u32))
                .unwrap();
            file.write_all(&f).unwrap();
            i += 1;
        }
        // write opcode mapping to source file
        file.write_all(&u32_to_bytes((self.opcode_span.len() * 3) as u32))
            .unwrap();
        for (ip, span) in &self.opcode_span {
            file.write_all(&u32_to_bytes(*ip as u32)).unwrap();
            file.write_all(&u32_to_bytes(span.0 as u32)).unwrap();
            file.write_all(&u32_to_bytes(span.1 as u32)).unwrap();
        }
    }

    fn compile_expr(&mut self, expr: &Spanned<Expr>) {
        self.span = (expr.0, expr.2);
        match expr.1.clone() {
            Expr::Literal { lit, ty: _ } => match lit {
                Lit::Int(i) => {
                    self.add_u64(CONST_U64, i as u64);
                }
                Lit::Double(d) => {
                    let bytes = d.to_bits();
                    self.add_u64(CONST_DOUBLE, bytes);
                }
                Lit::Char(c) => {
                    self.add_u8(CONST_U8, c);
                }
                Lit::Bool(b) => {
                    if b {
                        self.add(TRUE);
                    } else {
                        self.add(FALSE);
                    }
                }
                Lit::Str(s) => {
                    let idx = self.strings.len();
                    self.strings.push(s.clone());
                    self.add_u32(STRING, idx as u32);
                }
            },
            Expr::Variable { name, ty: _ } => {
                let local = self.get_local(&name);
                if local.is_some() {
                    let local = *local.unwrap();
                    self.add_u32(GET_LOCAL, local as u32);
                } else {
                    panic!("Unknown variable: {}", name);
                }
            }
            Expr::Neg { expr, ty } => {
                self.compile_expr(&expr);
                if ty.clone().unwrap() == Type::Double {
                    self.add(FNEG);
                } else {
                    self.add(INEG);
                }
            }
            Expr::Not { expr, ty: _ } => {
                self.compile_expr(&expr);
                self.add(NOT);
            }
            Expr::Arith {
                op,
                left: ex1,
                right: ex2,
                ty,
            } => {
                self.compile_expr(&ex1);
                self.compile_expr(&ex2);
                // if one of the operands is a double, use double arithmetic
                // also use I2F <i> to convert the int to a double...
                // ... this instruction would not pop the value from the stack but convert it in place
                // <i> denotes the index of the value on the stack from the top
                match op {
                    ArithOp::Add => {
                        let mut s_concat = false;
                        match ex1.1.get_type() {
                            Type::Str => {
                                if ty.clone().unwrap() == Type::Double {
                                    self.add_u32(F2S, 0);
                                }
                                s_concat = true;
                            }
                            _ => {}
                        }
                        match ex2.1.get_type() {
                            Type::Str => {
                                if ty.clone().unwrap() == Type::Double {
                                    self.add_u32(F2S, 1);
                                } else {
                                    self.add_u32(I2S, 0);
                                }
                                s_concat = true;
                            }
                            _ => {}
                        }
                        if s_concat {
                            self.add_u32(SCONCAT, 0);
                        } else {
                            if ty.clone().unwrap() == Type::Double {
                                self.add(FADD);
                            } else {
                                self.add(IADD);
                            }
                        }
                    }
                    ArithOp::Sub => {
                        if ty.clone().unwrap() == Type::Double {
                            self.add(FSUB);
                        } else {
                            self.add(ISUB);
                        }
                    }
                    ArithOp::Mul => {
                        if ty.clone().unwrap() == Type::Double {
                            self.add(FMUL);
                        } else {
                            self.add(IMUL);
                        }
                    }
                    ArithOp::Div => {
                        if ty.clone().unwrap() == Type::Double {
                            self.add(FDIV);
                        } else {
                            self.add(IDIV);
                        }
                    }
                    ArithOp::Mod => {
                        if ty.clone().unwrap() == Type::Double {
                            self.add(FMOD);
                        } else {
                            self.add(IMOD);
                        }
                    }
                }
            }
            Expr::BoolOp {
                op,
                left: ex1,
                right: ex2,
                ty: _,
            } => {
                self.compile_expr(&ex1);
                self.compile_expr(&ex2);
                match op {
                    BoolOp::And => {
                        self.add(AND);
                    }
                    BoolOp::Or => {
                        self.add(OR);
                    }
                }
            }
            Expr::CmpOp {
                op,
                left: ex1,
                right: ex2,
                ty,
            } => {
                self.compile_expr(&ex1);
                self.compile_expr(&ex2);
                match op {
                    CmpOp::Eq => {
                        self.add(EQUAL);
                    }
                    CmpOp::Ne => {
                        self.add(NOT_EQUAL);
                    }
                    CmpOp::Lt => {
                        if ex1.1.get_type()  == Type::Double {
                            self.add(FLESS);
                        } else {
                            self.add(ILESS);
                        }
                    }
                    CmpOp::Le => {
                        if ex1.1.get_type()  == Type::Double {
                            self.add(FLESS_EQUAL);
                        } else {
                            self.add(ILESS_EQUAL);
                        }
                    }
                    CmpOp::Gt => {
                        if ex1.1.get_type()  == Type::Double {
                            self.add(FGREATER);
                        } else {
                            self.add(IGREATER);
                        }
                    }
                    CmpOp::Ge => {
                        if ex1.1.get_type()  == Type::Double {
                            self.add(FGREATER_EQUAL);
                        } else {
                            self.add(IGREATER_EQUAL);
                        }
                    }
                }
            }
            Expr::BitOp {
                op,
                left,
                right,
                ty: _,
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                match op {
                    BitOp::And => {
                        self.add(BIT_AND);
                    }
                    BitOp::Or => {
                        self.add(BIT_OR);
                    }
                    BitOp::Xor => {
                        self.add(BIT_XOR);
                    }
                    BitOp::Shl => {
                        self.add(BIT_SHL);
                    }
                    BitOp::Shr => {
                        self.add(BIT_SHR);
                    }
                }
            }
            Expr::Cast { expr, ty } => {
                let exty = expr.1.get_type();
                self.compile_expr(&expr);
                self.add_2_u32(CAST, ty_to_int(exty), ty_to_int(ty));
            }
            Expr::Call { name, args, ty: _ } => {
                for arg in args.iter() {
                    self.compile_expr(&arg);
                }
                if name == "print" {
                    self.add_2_u32(NATIVE_CALL, 7, args.len() as u32);
                } else if name == "append" {
                    self.add(LIST_APPEND);
                } else if name == "len" {
                    self.add(LIST_LENGTH);
                } else if name == "clear" {
                    self.add_2_u32(NATIVE_CALL, 0, 0);
                } else if name == "rand" {
                    self.add_2_u32(NATIVE_CALL, 1, 2);
                } else if name == "sleep" {
                    self.add_2_u32(NATIVE_CALL, 2, 1);
                } else if name == "randf" {
                    self.add_2_u32(NATIVE_CALL, 3, 0);
                } else if name == "exp" {
                    self.add_2_u32(NATIVE_CALL, 4, 1);
                } else if name == "pow" {
                    self.add_2_u32(NATIVE_CALL, 5, 2);
                } else if name == "exit" {
                    self.add_2_u32(NATIVE_CALL, 6, 1);
                } else if name == "tanh" {
                    self.add_2_u32(NATIVE_CALL, 8, 1);
                } else if name == "log" {
                    self.add_2_u32(NATIVE_CALL, 9, 1);
                } else if name == "tcp_server" {
                    self.add_2_u32(NATIVE_CALL, 10, 2);
                } else if name == "close_chan" {
                    self.add_2_u32(NATIVE_CALL, 11, 1);
                } else if name == "is_open_chan" {
                    self.add_2_u32(NATIVE_CALL, 12, 1);
                } else {
                    let func_idx = self.func_idx[&name];
                    // println!("emitting call to {}", name);
                    let (call_offset, _) = self.add_2_u32(CALL, 0, args.len() as u32);
                    self.calls[func_idx].push((call_offset, self.curr_func));
                }
            }
            Expr::Subscr { expr, index, ty: _ } => {
                self.compile_expr(&expr);
                self.compile_expr(&index);
                self.add(LIST_INDEX);
            }
            Expr::Assign {
                left: lhs,
                right: rhs,
            } => {
                self.compile_expr(&rhs);
                match lhs.1 {
                    Expr::Variable { name, ty: _ } => {
                        let local = self.get_local(&name);
                        if local.is_some() {
                            let local = *local.unwrap();
                            self.add_u32(ASSIGN, local as u32);
                        } else {
                            panic!("Unknown variable: {}", name);
                        }
                    }
                    Expr::Subscr { expr, index, ty: _ } => {
                        self.compile_expr(&expr);
                        self.compile_expr(&index);
                        self.add(STORE)
                    }
                    _ => panic!("Invalid assignment"),
                }
            }
            Expr::Make { ty, expr: size } => {
                if size.is_some() {
                    self.compile_expr(size.as_ref().unwrap());
                }
                match ty {
                    Type::List(ty) => {
                        if size.is_none() {
                            self.add_u32(LIST, 0);
                        } else {
                            match *ty {
                                Type::Int => {
                                    self.add_u64(CONST_U64, 0);
                                }
                                Type::Double => {
                                    self.add_u64(CONST_DOUBLE, 0f64.to_bits());
                                }
                                Type::Char => {
                                    self.add_u8(CONST_U8, 0);
                                }
                                Type::Bool => {
                                    self.add(FALSE);
                                }
                                Type::Str => {
                                    self.add_u32(STRING, 0);
                                }
                                Type::List(_) => {
                                    self.add_u32(LIST, 0);
                                }
                                _ => panic!("Cannot make a non-list"),
                            }
                            self.add(MAKE_LIST);
                        }
                    }
                    Type::Chan(_) => {
                        self.add(MAKE_CHAN);
                    }
                    Type::Map(_, _) => {
                        self.add(MAKE_MAP);
                    }
                    _ => {
                        panic!("Can only make a list or a chan");
                    }
                }
            }
            Expr::Tuple { exprs, ty: _ } => {
                for expr in exprs.iter() {
                    self.compile_expr(&expr);
                }
                self.add_u32(MAKE_TUPLE, exprs.len() as u32);
            }
            Expr::ChanRead { name, ty: _ } => {
                let local = self.get_local(&name).unwrap();
                self.add_u32(GET_LOCAL, *local as u32);
                self.add(CHAN_READ);
            }
            Expr::List { exprs, ty: _ } => {
                for ex in exprs.iter() {
                    self.compile_expr(&ex);
                }
                self.add_u32(LIST, exprs.len() as u32);
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn incr_for_loop(&mut self, idx: usize, start: usize) {
        self.add_u32(GET_LOCAL, idx as u32);
        self.add_u64(CONST_U64, 1);
        self.add(IADD);
        self.add_u32(ASSIGN, idx as u32);
        self.add_u32(JUMP, (start + 1) as u32);
    }

    fn step_range_loop(&mut self, idx: usize, start: usize, forw_var: usize) {
        self.add_u32(GET_LOCAL, forw_var as u32);
        let jump = self.add_u32(JUMP_IF_FALSE, 0);
        self.add_u32(GET_LOCAL, idx as u32);
        self.add_u64(CONST_U64, 1);
        self.add(IADD);
        self.add_u32(ASSIGN, idx as u32);
        self.add_u32(JUMP, start as u32);
        self.patch_u32(jump, self.code.len() as u32);
        self.add_u32(GET_LOCAL, idx as u32);
        self.add_u64(CONST_U64, 1);
        self.add(ISUB);
        self.add_u32(ASSIGN, idx as u32);
        self.add_u32(JUMP, start as u32);
    }

    fn compile_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.1 {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
                // self.add(POP);
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr);
                self.add(RETURN);
            }
            Stmt::IfElse(cond, then, otherwise) => {
                self.compile_expr(cond);
                let jump = self.add_u32(JUMP_IF_FALSE, 0);
                for stmt in then {
                    self.compile_stmt(stmt);
                }
                let jump2 = self.add_u32(JUMP, 0);
                self.patch_u32(jump, self.code.len() as u32);
                // println!("patched jumpIfFalse at {} to {}", jump, self.code.len());
                if otherwise.is_some() {
                    let otherwise = otherwise.as_ref().unwrap();
                    for stmt in otherwise {
                        self.compile_stmt(stmt);
                    }
                }
                // println!("patching jump at {} to {}", jump2, self.code.len());
                self.patch_u32(jump2, self.code.len() as u32);
            }
            Stmt::While(cond, body) => {
                let start = self.code.len();
                self.compile_expr(cond);
                let parent = self.loop_state.clone();
                self.loop_state = LoopState {
                    breaks: vec![],
                    loop_type: LoopType::While,
                    loop_start: Some(start),
                    loop_end_var: None,
                    loop_forw_var: None,
                    idx_var: None,
                };
                let jump = self.add_u32(JUMP_IF_FALSE, 0);
                for stmt in body {
                    self.compile_stmt(stmt);
                }
                self.add_u32(JUMP, start as u32);
                self.patch_u32(jump, self.code.len() as u32);
                for break_ in self.loop_state.breaks.clone() {
                    self.patch_u32(break_, self.code.len() as u32);
                }
                self.loop_state = parent;
            }
            Stmt::For(var, expr, body) => {
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                } = expr.1.clone()
                {
                    self.compile_expr(&start);
                    let start_var = self.add_local("<start>");
                    self.add_u32(DEF_LOCAL, start_var as u32);
                    self.compile_expr(&end);
                    if inclusive {
                        self.add_u64(CONST_U64, 1);
                        self.add(IADD);
                    }
                    let end_var = self.add_local("<end>");
                    self.add_u32(DEF_LOCAL, end_var as u32);
                    self.add_u32(GET_LOCAL, start_var as u32);
                    let idx_var = self.add_local(var.as_str());
                    self.add_u32(DEF_LOCAL, idx_var as u32);
                    let forw_var = self.add_local("<forw>");
                    self.add_u32(GET_LOCAL, start_var as u32);
                    self.add_u32(GET_LOCAL, end_var as u32);
                    if inclusive {
                        self.add(ILESS);
                    } else {
                        self.add(ILESS_EQUAL);
                    }
                    self.add_u32(DEF_LOCAL, forw_var as u32);
                    self.add_u32(GET_LOCAL, idx_var as u32);
                    self.add_u32(GET_LOCAL, end_var as u32);
                    let start = self.code.len();
                    let parent = self.loop_state.clone();
                    self.loop_state = LoopState {
                        breaks: vec![],
                        loop_type: LoopType::For,
                        loop_start: Some(start),
                        loop_end_var: Some(end_var),
                        loop_forw_var: Some(forw_var),
                        idx_var: Some(idx_var),
                    };
                    self.add_u32(GET_LOCAL, idx_var as u32);
                    self.add_u32(GET_LOCAL, end_var as u32);
                    self.add(NOT_EQUAL);
                    let jump = self.add_u32(JUMP_IF_FALSE, 0);
                    for stmt in body {
                        self.compile_stmt(stmt);
                    }
                    self.step_range_loop(idx_var, start, forw_var);
                    self.patch_u32(jump, self.code.len() as u32);
                    for break_ in self.loop_state.breaks.clone() {
                        self.patch_u32(break_, self.code.len() as u32);
                    }
                    self.loop_state = parent;
                } else {
                    let arr_var = if let Expr::Variable { name, ty: _ } = expr.1.clone() {
                        *self.get_local(name.as_str()).unwrap()
                    } else {
                        self.compile_expr(expr);
                        let arr = self.add_local("<arr>");
                        self.add_u32(DEF_LOCAL, arr as u32);
                        arr
                    };
                    self.add_u32(GET_LOCAL, arr_var as u32);
                    self.add(LIST_LENGTH);
                    let len_var = self.add_local("<len>");
                    self.add_u32(DEF_LOCAL, len_var as u32);
                    let idx_var = self.add_local("<idx>");
                    self.add_u64(CONST_U64, 0);
                    self.add_u32(DEF_LOCAL, idx_var as u32);
                    let start = self.code.len() - 1;
                    let parent = self.loop_state.clone();
                    self.loop_state = LoopState {
                        loop_start: Some(start),
                        loop_type: LoopType::For,
                        breaks: Vec::new(),
                        loop_end_var: None,
                        loop_forw_var: None,
                        idx_var: Some(idx_var),
                    };
                    self.add_u32(GET_LOCAL, idx_var as u32);
                    self.add_u32(GET_LOCAL, len_var as u32);
                    self.add(ILESS);
                    let jump = self.add_u32(JUMP_IF_FALSE, 0);
                    let var_var = self.add_local(&var.clone());
                    self.add_u32(GET_LOCAL, arr_var as u32);
                    self.add_u32(GET_LOCAL, idx_var as u32);
                    self.add(LIST_INDEX);
                    self.add_u32(DEF_LOCAL, var_var as u32);
                    for stmt in body {
                        self.compile_stmt(stmt);
                    }
                    self.incr_for_loop(idx_var, start);
                    self.patch_u32(jump, self.code.len() as u32);
                    let len = self.code.len();
                    for break_ in self.loop_state.breaks.clone() {
                        self.patch_u32(break_, len as u32);
                    }
                    self.loop_state = parent;
                }
            }
            Stmt::Decl(name, expr) => {
                let n = self.add_local(name);
                self.compile_expr(expr);
                self.add_u32(DEF_LOCAL, n as u32);
            }
            Stmt::Break => {
                if self.loop_state.loop_type == LoopType::None {
                    panic!("Break outside of loop");
                }
                let jump = self.add_u32(JUMP, 0);
                self.loop_state.breaks.push(jump);
            }
            Stmt::Continue => {
                if self.loop_state.loop_type == LoopType::None {
                    panic!("Continue outside of loop");
                } else {
                    let start = self.loop_state.loop_start.unwrap();
                    if self.loop_state.loop_type == LoopType::For {
                        let idx_var = self.loop_state.idx_var.unwrap();
                        self.incr_for_loop(idx_var, start);
                    } else if self.loop_state.loop_type == LoopType::Range {
                        let idx_var = self.loop_state.idx_var.unwrap();
                        let forw_var = self.loop_state.loop_forw_var.unwrap();
                        self.step_range_loop(idx_var, start, forw_var);
                    } else {
                        self.add_u32(JUMP, start as u32);
                    }
                }
            }
            Stmt::Comment => {
                // do nothing
            }
            Stmt::Coroutine(call) => {
                self.add(SPAWN);
                self.compile_expr(call);
            }
            Stmt::ChanWrite(name, expr) => {
                self.compile_expr(expr);
                let local = *self.get_local(name).unwrap();
                self.add_u32(GET_LOCAL, local as u32);
                self.add(CHAN_WRITE)
            }
        }
    }

    fn compile_fn(&mut self, args: &[String], body: &[Spanned<Stmt>]) {
        for arg in args {
            self.add_local(arg);
        }
        for stmt in body {
            self.compile_stmt(stmt);
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        let fns = &program.funcs;
        self.calls = (0..fns.len() + 1).map(|_| Vec::new()).collect();
        let mut idx = 0;
        for func in fns {
            self.func_idx.insert(func.name.clone(), idx);
            idx += 1;
        }
        idx = 0;
        let mut func_code = Vec::new();
        for func in fns {
            self.curr_func = idx;
            self.locals.clear();
            let args = &func
                .args
                .iter()
                .map(|arg| arg.name.clone())
                .collect::<Vec<_>>();
            self.func_args_len.push(args.len() as u32);
            let body = &func.body;
            if func.name == "main" {
                self.start_func_idx = idx;
            }
            self.compile_fn(args, body);
            self.add(RETURN);
            func_code.push(self.code.clone());
            self.code.clear();
            idx += 1;
        }
        self.func_code = func_code;
        self.locals.clear();
        for func in fns {
            let func_idx = self.func_idx[&func.name];
            for (offset, calling_fn) in &self.calls[func_idx] {
                self.func_code[*calling_fn][*offset + 3] = (func_idx >> 24) as u8;
                self.func_code[*calling_fn][*offset + 2] = (func_idx >> 16) as u8;
                self.func_code[*calling_fn][*offset + 1] = (func_idx >> 8) as u8;
                self.func_code[*calling_fn][*offset] = func_idx as u8;
            }
        }
    }
}

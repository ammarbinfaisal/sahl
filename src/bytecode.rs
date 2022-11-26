use crate::syntax::*;
use std::collections::HashMap;
use std::fs::*;
use std::io::Write;

const ADD: u8 = 0;
const SUB: u8 = 1;
const MUL: u8 = 2;
const DIV: u8 = 3;
const MOD: u8 = 4;
const NEG: u8 = 5;
const NOT: u8 = 6;
const AND: u8 = 7;
const OR: u8 = 8;
const EQUAL: u8 = 9;
const NOT_EQUAL: u8 = 10;
const LESS: u8 = 11;
const LESS_EQUAL: u8 = 12;
const GREATER: u8 = 13;
const GREATER_EQUAL: u8 = 14;
const TRUE: u8 = 15;
const FALSE: u8 = 16;
const JUMP: u8 = 17;
const JUMP_IF_FALSE: u8 = 18;
const STORE: u8 = 19;
const INDEX: u8 = 20;
const APPEND: u8 = 21;
const LENGTH: u8 = 22;
const LIST: u8 = 23;
const CONST_U64: u8 = 24;
const CONST_U32: u8 = 25;
const CONST_U8: u8 = 26;
const STRING: u8 = 27;
const DEF_LOCAL: u8 = 28;
const GET_LOCAL: u8 = 29;
const ASSIGN: u8 = 30;
const CALL: u8 = 31;
const RETURN: u8 = 32;
const PRINT: u8 = 33;
const POP: u8 = 34;
const MAKE_LIST: u8 = 35;

const MAX_LOCALS: usize = (i64::pow(2, 32) - 1) as usize;

pub struct Bytecode {
    code: Vec<u8>,
    locals: HashMap<String, usize>,
    func_idx: HashMap<String, usize>,
    calls: Vec<Vec<usize>>, // to be patched after codegen - offset
    start_ip: usize,
}

impl Bytecode {
    pub fn new() -> Bytecode {
        Bytecode {
            code: Vec::new(),
            locals: HashMap::new(),
            func_idx: HashMap::new(),
            calls: Vec::new(),
            start_ip: 0,
        }
    }

    fn add(&mut self, op: u8) {
        self.code.push(op);
    }

    fn add_u8(&mut self, op: u8, arg: u8) -> usize {
        self.code.push(op);
        self.code.push(arg);
        self.code.len() - 1
    }

    fn add_u32(&mut self, op: u8, n: u32) -> usize {
        self.code.push(op);
        self.code.push(n as u8);
        self.code.push((n >> 8) as u8);
        self.code.push((n >> 16) as u8);
        self.code.push((n >> 24) as u8);
        self.code.len() - 4
    }

    fn add_u64(&mut self, op: u8, n: u64) -> usize {
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
        [
            self.start_ip as u8,
            (self.start_ip >> 8) as u8,
            (self.start_ip >> 16) as u8,
            (self.start_ip >> 24) as u8,
        ]
    }

    pub fn write(&self, path: &str) {
        let mut file = File::create(path).unwrap();
        file.write_all(&self.header()).unwrap();
        file.write_all(&self.code).unwrap();
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(lit) => match lit {
                Lit::Int(i) => {
                    self.add_u64(CONST_U64, *i as u64);
                }
                Lit::Char(c) => {
                    self.add_u8(CONST_U8, *c);
                }
                Lit::Bool(b) => {
                    if *b {
                        self.add(TRUE);
                    } else {
                        self.add(FALSE);
                    }
                }
                Lit::Str(s) => {
                    self.add(STRING);
                    self.add_u32(CONST_U32, s.len() as u32);
                    for c in s {
                        self.add(*c);
                    }
                }
                Lit::List(l) => {
                    for expr in l {
                        self.compile_expr(expr);
                    }
                    self.add_u32(LIST, l.len() as u32);
                }
            },
            Expr::Variable(name) => {
                let local = self.get_local(name);
                if local.is_some() {
                    let local = *local.unwrap();
                    self.add_u32(GET_LOCAL, local as u32);
                } else {
                    panic!("Unknown variable: {}", name);
                }
            }
            Expr::Neg(e) => {
                self.compile_expr(e);
                self.add(NEG);
            }
            Expr::Not(e) => {
                self.compile_expr(e);
                self.add(NOT);
            }
            Expr::Arith(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    ArithOp::Add => {
                        self.add(ADD);
                    }
                    ArithOp::Sub => {
                        self.add(SUB);
                    }
                    ArithOp::Mul => {
                        self.add(MUL);
                    }
                    ArithOp::Div => {
                        self.add(DIV);
                    }
                    ArithOp::Mod => {
                        self.add(MOD);
                    }
                }
            }
            Expr::BoolOp(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    BoolOp::And => {
                        self.add(AND);
                    }
                    BoolOp::Or => {
                        self.add(OR);
                    }
                }
            }
            Expr::CmpOp(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    CmpOp::Eq => {
                        self.add(EQUAL);
                    }
                    CmpOp::Ne => {
                        self.add(NOT_EQUAL);
                    }
                    CmpOp::Lt => {
                        self.add(LESS);
                    }
                    CmpOp::Le => {
                        self.add(LESS_EQUAL);
                    }
                    CmpOp::Gt => {
                        self.add(GREATER);
                    }
                    CmpOp::Ge => {
                        self.add(GREATER_EQUAL);
                    }
                }
            }
            Expr::Call(name, args) => {
                if name == "print" {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.add(PRINT);
                } else if name == "append" {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.add(APPEND);
                } else {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let func_idx = self.func_idx[name];
                    // println!("emitting call to {}", name);
                    let (call_offset, _) = self.add_2_u32(CALL, 0, args.len() as u32);
                    self.calls[func_idx].push(call_offset);
                }
            }
            Expr::Subscr(e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                self.add(INDEX);
            }
            Expr::Assign(lhs, rhs) => {
                self.compile_expr(rhs);
                match &**lhs {
                    Expr::Variable(name) => {
                        let local = self.get_local(&name);
                        if local.is_some() {
                            let local = *local.unwrap();
                            self.add_u32(ASSIGN, local as u32);
                        } else {
                            panic!("Unknown variable: {}", name);
                        }
                    }
                    Expr::Subscr(e1, e2) => {
                        self.compile_expr(e1);
                        self.compile_expr(e2);
                        self.add(STORE)
                    }
                    _ => panic!("Invalid assignment"),
                }
            }
            Expr::Make(ty, size) => {
                if size.is_some() {
                    self.compile_expr(size.as_ref().unwrap());
                }
                match ty {
                    Type::List(ty) => {
                        if size.is_none() {
                            self.add_u32(LIST, 0);
                        } else {
                            match **ty {
                                Type::Int => {
                                    self.add_u64(CONST_U64, 0);
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
                                _ => {
                                    panic!("Invalid type for list");
                                }
                            }
                            self.add(MAKE_LIST);
                        }
                    }
                    _ => panic!("Cannot make a non-list"),
                }
            }
            _ => {
                println!("unimplemented {:?}", expr);
            }
        }
    }

    fn incr_for_loop(&mut self, idx: usize, start: usize) {
        self.add_u32(GET_LOCAL, idx as u32);
        self.add_u64(CONST_U64, 1);
        self.add(ADD);
        self.add_u32(ASSIGN, idx as u32);
        self.add_u32(JUMP, (start + 1) as u32);
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
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
                let jump = self.add_u32(JUMP_IF_FALSE, 0);
                let mut breaks = Vec::<usize>::new();
                for stmt in body {
                    if stmt == &Stmt::Break {
                        breaks.push(self.add_u32(JUMP, 0));
                    } else if stmt == &Stmt::Continue {
                        self.add_u32(JUMP, start as u32);
                    } else {
                        self.compile_stmt(stmt);
                    }
                }
                self.add_u32(JUMP, start as u32);
                self.patch_u32(jump, self.code.len() as u32);
                for break_ in breaks {
                    self.patch_u32(break_, self.code.len() as u32);
                }
            }
            Stmt::For(var, expr, body) => {
                self.compile_expr(expr);
                let arr_var = self.add_local("<arr>");
                self.add_u32(DEF_LOCAL, arr_var as u32);
                self.add_u32(GET_LOCAL, arr_var as u32);
                self.add(LENGTH);
                let len_var = self.add_local("<len>");
                self.add_u32(DEF_LOCAL, len_var as u32);
                let idx_var = self.add_local("<idx>");
                self.add_u64(CONST_U64, 0);
                self.add_u32(DEF_LOCAL, idx_var as u32);
                let start = self.code.len() - 1;
                self.add_u32(GET_LOCAL, idx_var as u32);
                self.add_u32(GET_LOCAL, len_var as u32);
                self.add(LESS);
                let jump = self.add_u32(JUMP_IF_FALSE, 0);
                let var_var = self.add_local(&var.clone());
                self.add_u32(GET_LOCAL, arr_var as u32);
                self.add_u32(GET_LOCAL, idx_var as u32);
                self.add(INDEX);
                self.add_u32(DEF_LOCAL, var_var as u32);
                let mut breaks = Vec::<usize>::new();
                for stmt in body {
                    if *stmt == Stmt::Break {
                        breaks.push(self.add_u32(JUMP, 0));
                    } else if *stmt == Stmt::Continue {
                        self.incr_for_loop(arr_var, start);
                    } else {
                        self.compile_stmt(stmt);
                    }
                }
                self.incr_for_loop(idx_var, start);
                self.patch_u32(jump, self.code.len() as u32);
                let len = self.code.len();
                for break_ in breaks {
                    self.patch_u32(break_, len as u32);
                }
            }
            Stmt::Decl(name, expr) => {
                let n = self.add_local(name);
                self.compile_expr(expr);
                self.add_u32(DEF_LOCAL, n as u32);
            }
            Stmt::Break => {
                panic!("Break outside of loop");
            }
            Stmt::Continue => {
                panic!("Continue outside of loop");
            }
            Stmt::Comment => {
                // do nothing
            }
            Stmt::Coroutine(call) => {
                self.compile_expr(call);
            }
            _ => {
                println!("unimplemented {:?}", stmt);
            }
        }
    }

    fn compile_fn(&mut self, args: &[String], body: &[Stmt]) {
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
        let mut func_ips = Vec::<usize>::new();
        for func in fns {
            func_ips.push(self.code.len());
            self.locals.clear();
            let args = &func
                .args
                .iter()
                .map(|arg| arg.name.clone())
                .collect::<Vec<_>>();
            let body = &func.body;
            if func.name == "main" {
                self.start_ip = self.code.len();
            }
            self.compile_fn(args, body);
            self.add(RETURN);
        }
        self.locals.clear();
        for func in fns {
            let func_idx = self.func_idx[&func.name];
            let func_ip = func_ips[func_idx];
            for offset in &self.calls[func_idx] {
                self.code[*offset+3] = (func_ip >> 24) as u8;
                self.code[*offset+2] = (func_ip >> 16) as u8;
                self.code[*offset+1] = (func_ip >> 8) as u8;
                self.code[*offset] = func_ip as u8;
            }
        }
        println!("start ip: {}", self.start_ip);
        println!("code length: {}", self.code.len());
        // println!("code: {:?}", self.code);
    }
}

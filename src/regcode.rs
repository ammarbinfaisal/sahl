use crate::syntax::*;
use std::collections::HashMap;

// highlevel enum for 3/4 address code
#[derive(Debug, Clone)]
pub enum RegCode {
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
    Make(Type, u8, Option<u8>),
    // lists
    ListSet(u8, u8, u8),
    ListGet(u8, u8, u8),
    List(usize, u8), // length, reg
    // tuples
    TupleGet(u8, u8, u8),
    Tuple(usize, u8), // length, reg
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
    Const(usize, u8), // const_idx, reg
    Load(usize, u8),
    Store(usize, u8),
    Cast(u8, Type, Type, u8),
    Move(u8, u8),
    Return(u8),
    Push(u8),
    Spawn,
    Nop,
    Phi(usize),
    FreeRegs,
}

pub struct RegCodeGen<'a> {
    code: Vec<RegCode>,
    locals: HashMap<&'a str, usize>,
    func_idx: HashMap<&'a str, usize>,
    func_args_len: Vec<u32>,
    pub func_code: Vec<Vec<RegCode>>,
    pub start_func_idx: usize,
    curr_func: usize,
    opcode_span: HashMap<usize, (usize, usize)>,
    span: (usize, usize),
    source_name: String,
    pub consts: Vec<(Type, Vec<u8>)>,
    stack: Vec<u8>,
    free_regs: [bool; 256],
    breaks: Vec<Vec<usize>>,
    loop_starts: Vec<usize>, // stack of (start, end) of loops
    pub local_counts: Vec<usize>,
}

impl<'a> RegCodeGen<'a> {
    pub fn new(source_name: String) -> Self {
        RegCodeGen {
            code: Vec::new(),
            locals: HashMap::new(),
            func_idx: HashMap::new(),
            func_code: Vec::new(),
            func_args_len: Vec::new(),
            loop_starts: Vec::new(),
            start_func_idx: 0,
            curr_func: 0,
            opcode_span: HashMap::new(),
            consts: Vec::new(),
            span: (0, 0),
            source_name,
            stack: Vec::new(),
            free_regs: [true; 256],
            breaks: Vec::new(),
            local_counts: Vec::new(),
        }
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
            "append" => Some((2, 2, false)),
            "assert" => Some((3, 2, false)),
            "len" => Some((4, 1, true)),
            _ => None,
        }
    }

    fn stack_pop(&mut self) -> u8 {
        let reg = self.stack.pop().unwrap();
        println!("stack pop: {}", reg);
        self.free_reg(reg);
        reg
    }

    fn stack_push(&mut self, reg: u8) {
        self.stack.push(reg);
        self.free_regs[reg as usize] = false;
    }

    fn stack_unfree_pop(&mut self) -> u8 {
        let reg = self.stack.pop().unwrap();
        println!("stack unfree pop: {}", reg);
        reg
    }

    fn compile_expr(&mut self, expr: &Spanned<Expr>) {
        println!("compiling expr: {:?}", expr);
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
                        self.consts.push((Type::Char, c.to_le_bytes().to_vec()));
                    }
                    Lit::Bool(b) => {
                        if *b {
                            self.consts.push((Type::Bool, vec![1]));
                        } else {
                            self.consts.push((Type::Bool, vec![0]));
                        }
                    }
                    Lit::Str(s) => {
                        let mut s = s.clone();
                        s.push(0);
                        self.consts.push((Type::Str, s.clone()));
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
                    self.code.push(RegCode::Load(local, reg));
                    self.stack_push(reg);
                } else {
                    panic!("Unknown variable: {}", name);
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
                let arg2 = self.stack_pop();
                let arg1 = self.stack_pop();
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
            }
            Expr::BoolOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_pop();
                let arg1 = self.stack_pop();
                let op = match op {
                    BoolOp::And => RegCode::LAnd,
                    BoolOp::Or => RegCode::LOr,
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
            }
            Expr::BitOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg2 = self.stack_pop();
                let arg1 = self.stack_pop();
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
            }
            Expr::CmpOp {
                op, left, right, ..
            } => {
                self.compile_expr(&left);
                self.compile_expr(&right);
                let arg1 = self.stack_pop();
                let arg2 = self.stack_pop();
                let ty = left.1.get_type();
                let op = if ty == Type::Int {
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
            }
            Expr::Neg { expr, ty } => {
                self.compile_expr(&expr);
                let arg = self.stack_pop();
                let op = if ty.clone().unwrap() == Type::Int {
                    RegCode::INeg
                } else {
                    RegCode::FNeg
                };
                let reg = self.get_reg();
                self.code.push(op(arg, reg));
                self.stack_push(reg);
            }
            Expr::Not { expr, .. } => {
                self.compile_expr(&expr);
                let arg = self.stack_pop();
                let reg = self.get_reg();
                self.code.push(RegCode::LNot(arg, reg));
                self.stack_push(reg);
            }
            Expr::Call { name, args, ty } => {
                for arg in args {
                    self.compile_expr(&arg);
                }
                let mut arg_regs = vec![];
                for _ in 0..args.len() {
                    arg_regs.push(self.stack.pop().unwrap());
                }
                if name == "print" {
                    self.code.push(RegCode::NCall(0, arg_regs));
                    return;
                } else if name == "println" {
                    self.code.push(RegCode::NCall(1, arg_regs));
                    return;
                } else if let Some((native_ix, _arity, returns)) = self.builtin(name) {
                    self.code.push(RegCode::NCall(native_ix, arg_regs));
                    if returns {
                        let reg = self.get_reg();
                        self.code.push(RegCode::Move(0, reg));
                        self.stack_push(reg);
                    }
                    return;
                }
                let reg = self.get_reg();
                let func = self.func_idx.get(name.as_str());
                if func.is_some() {
                    let func = *func.unwrap();
                    self.code.push(RegCode::Call(func, arg_regs));
                    if ty.is_some() {
                        self.code.push(RegCode::Move(0, reg));
                        self.stack_push(reg);
                    } else {
                        self.free_reg(reg);
                    }
                } else {
                    panic!("Unknown function: {}", name);
                }
            }
            Expr::Subscr { expr, index, .. } => {
                self.compile_expr(&expr);
                self.compile_expr(&index);
                let arg1 = self.stack_pop();
                let arg2 = self.stack_pop();
                let op = match expr.1.get_type() {
                    Type::List(_) => RegCode::ListGet,
                    Type::Map(_, _) => RegCode::MapGet,
                    Type::Str => RegCode::StrGet,
                    Type::Tuple(_) => RegCode::TupleGet,
                    _ => unreachable!("Unknown type: {:?}", expr.1.get_type()),
                };
                let reg = self.get_reg();
                self.code.push(op(arg1, arg2, reg));
                self.stack_push(reg);
            }
            Expr::Assign { left, right } => {
                self.compile_expr(&right);
                let arg = self.stack_pop();
                match &(*left).1 {
                    Expr::Variable { name, .. } => {
                        let var = self.get_local(name);
                        match var {
                            Some(var) => {
                                self.code.push(RegCode::Store(*var, arg));
                            }
                            None => {
                                unreachable!("Unknown variable: {}", name);
                            }
                        }
                    }
                    Expr::Subscr { expr, index, .. } => {
                        self.compile_expr(expr);
                        let ex_arg = self.stack_pop();
                        self.compile_expr(index);
                        let ix_arg = self.stack_pop();
                        let op = match (*expr).1.get_type() {
                            Type::List(_) => RegCode::ListSet,
                            Type::Map(_, _) => RegCode::MapSet,
                            _ => unreachable!(),
                        };
                        self.code.push(op(ex_arg, ix_arg, arg));
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Make { ty, expr: size } => {
                let reg = self.get_reg();
                let size = size
                    .into_iter()
                    .map(|e| {
                        self.compile_expr(&e);
                        self.stack_pop()
                    })
                    .next();
                self.code.push(RegCode::Make(ty.clone(), reg, size));
                self.stack_push(reg);
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
                for expr in exprs {
                    self.compile_expr(&expr);
                    let reg = self.stack_pop();
                    self.code.push(RegCode::Push(reg));
                }
                let reg = self.get_reg();
                self.code.push(RegCode::Tuple(exprs.len(), reg));
                self.stack_push(reg);
            }
            Expr::List { exprs, .. } => {
                for expr in exprs {
                    self.compile_expr(&expr);
                    let reg = self.stack_pop();
                    self.code.push(RegCode::Push(reg));
                }
                let reg = self.get_reg();
                self.code.push(RegCode::List(exprs.len(), reg));
                self.stack_push(reg);
            }
            Expr::ChanRead { name, .. } => {
                let reg = self.get_reg();
                let chan = self.locals.get(name.as_str()).unwrap();
                self.code.push(RegCode::ChanRecv(*chan, reg));
                self.stack_push(reg);
            }
            _ => {
                unimplemented!();
            }
        }
        println!("stack: {:?}", self.stack);
    }

    fn compile_stmt(&mut self, stmt: &'a Spanned<Stmt>) {
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
                for stmt in then {
                    self.compile_stmt(&stmt);
                }
                let jmp_ix2 = self.code.len();
                self.code.push(RegCode::Nop);
                self.code[jmp_ix] = RegCode::JmpIfNot(arg, jmp_ix2 + 1);
                if let Some(else_body) = els {
                    for stmt in else_body {
                        self.compile_stmt(&stmt);
                    }
                }
                let jmp_ix3 = self.code.len();
                self.code[jmp_ix2] = RegCode::Jmp(jmp_ix3);
                self.free_reg(arg);
            }
            Stmt::Decl(var, expr) => {
                self.compile_expr(&expr);
                let lcl = self.locals.len();
                self.locals.insert(var.as_str(), lcl);
                let arg = self.stack_pop();
                self.code.push(RegCode::Store(lcl, arg));
            }
            Stmt::For(var, expr, body) => {
                // if expr is Range then set var to range.start and loop until range.end
                // ele introduce new variable and loop until expr.len
                let var_ix = self.locals.len();
                self.breaks.push(vec![]);
                self.locals.insert(var.as_str(), var_ix);
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                } = &(*expr).1
                {
                    self.compile_expr(&start);
                    let start = self.stack_unfree_pop();
                    self.code.push(RegCode::Store(var_ix, start));

                    let start_ix = self.code.len();
                    self.compile_expr(&end);
                    let end = self.stack_unfree_pop();
                    let cond_reg = self.get_reg();
                    let iter = self.get_reg();
                    let jmp_ix = self.code.len();
                    if *inclusive {
                        self.code.push(RegCode::ILe(iter, end, cond_reg));
                    } else {
                        self.code.push(RegCode::ILt(iter, end, cond_reg));
                    };
                    let jmp_ix2 = self.code.len();
                    self.code.push(RegCode::JmpIfNot(cond_reg, jmp_ix2 + 1));
                    let jmp_ix3 = self.code.len();
                    self.code.push(RegCode::Nop);
                    self.loop_starts.push(start_ix);
                    for stmt in body {
                        self.compile_stmt(&stmt);
                    }
                    self.code.push(RegCode::IAdd(iter, 1, iter));
                    self.code.push(RegCode::Jmp(jmp_ix));
                    let jmp_ix4 = self.code.len();
                    self.code[jmp_ix3] = RegCode::JmpIfNot(cond_reg, jmp_ix4);
                    self.free_reg(iter);
                    self.free_reg(cond_reg);
                    let loop_ix: usize = self.breaks.len() - 1;
                    for ix in self.breaks[loop_ix].iter() {
                        self.code[*ix] = RegCode::Jmp(jmp_ix4);
                    }
                    self.loop_starts.pop();
                } else {
                    self.compile_expr(&expr);
                    let arg = self.stack_unfree_pop();
                    let iter = self.get_reg();
                    let cond_reg = self.get_reg();
                    let jmp_ix = self.code.len();
                    self.code.push(RegCode::JmpIfNot(iter, jmp_ix + 2));
                    let jmp_ix2 = self.code.len();
                    self.code.push(RegCode::Nop);
                    // assign var
                    let reg = self.get_reg();
                    self.code.push(RegCode::ListGet(arg, iter, reg));
                    self.code.push(RegCode::Store(var_ix, reg));
                    self.free_reg(reg);
                    let v = self.get_local(&var);
                    match v {
                        Some(var) => {
                            self.code.push(RegCode::Store(*var, iter));
                        }
                        None => {
                            unreachable!("Unknown variable: {}", var);
                        }
                    }
                    for stmt in body {
                        self.compile_stmt(&stmt);
                    }
                    self.code.push(RegCode::IAdd(iter, 1, iter));
                    self.code.push(RegCode::Jmp(jmp_ix));
                    let jmp_ix3 = self.code.len();
                    self.code[jmp_ix2] = RegCode::JmpIfNot(cond_reg, jmp_ix3);
                    self.free_reg(iter);
                    let loop_ix: usize = self.breaks.len() - 1;
                    for ix in self.breaks[loop_ix].iter() {
                        self.code[*ix] = RegCode::Jmp(jmp_ix3);
                    }
                    self.loop_starts.pop();
                }
                self.breaks.pop();
                self.locals.remove(var.as_str());
            }
            Stmt::While(cond, body) => {
                let jmp_ix = self.code.len();
                self.compile_expr(&cond);
                self.breaks.push(vec![]);
                let arg = self.stack_unfree_pop();
                let jmp_ix2 = self.code.len();
                self.loop_starts.push(jmp_ix);
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
                self.breaks.pop();
            }
            Stmt::Return(expr) => {
                self.compile_expr(&expr);
                let arg = self.stack_pop();
                self.code.push(RegCode::Return(arg));
            }
            Stmt::Break => {
                let jmp_ix = self.code.len();
                self.code.push(RegCode::Nop);
                let loop_ix: usize = self.breaks.len() - 1;
                self.breaks[loop_ix].push(jmp_ix);
            }
            Stmt::Continue => {
                let start_ix = self.loop_starts.last().unwrap();
                self.code.push(RegCode::Jmp(*start_ix));
            }
            Stmt::ChanWrite(var, expr) => {
                self.compile_expr(&expr);
                let arg = self.stack_pop();
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
                self.code.push(RegCode::Spawn);
                self.compile_expr(&expr);
            }
        }
    }

    fn compile_func(&mut self, func: &'a Func) {
        for arg in &func.args {
            let lcl = self.locals.len();
            self.locals.insert(arg.name.as_str(), lcl);
        }
        self.curr_func = self.func_idx[&func.name.as_str()];
        for stmt in &func.body {
            self.compile_stmt(&stmt);
        }
    }

    pub fn compile_program(&mut self, prog: &'a Program) {
        let fns = &prog.funcs;
        let mut idx = 0;
        for func in fns {
            self.func_idx.insert(func.name.as_str(), idx);
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
            if func.name == "main" {
                self.start_func_idx = idx;
            }
            self.compile_func(func);
            func_code.push(self.code.clone());
            self.code.clear();
            self.local_counts.push(self.locals.len());
            idx += 1;
        }
        self.func_code = func_code;
        self.locals.clear();
    }
}

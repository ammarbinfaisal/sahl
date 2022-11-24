use crate::syntax::*;
use crate::vm::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Instruction {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    True,
    False,
    Jump(usize),
    JumpIfFalse(usize),
    Store,
    Index,
    Append,
    Length,
    MakeList(usize, Value),
    List(usize),
    Const(Value),
    DefLocal(usize),
    GetLocal(usize),
    Assign(usize),
    Call(usize, usize),   // global, num_args
    Coroutine,
    MakeChan,
    ChanRead,
    ChanWrite,
    Return,
    Print,
    Pop,
}

pub struct Codegen {
    num_locals: usize,
    instructions: Vec<Instruction>,
    locals: HashMap<String, usize>,
    functions: HashMap<String, usize>,
    calls: Vec<Vec<usize>>,
    func_idx: HashMap<String, usize>,
    curr_func: usize,
    start_ip: usize,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            num_locals: 0,
            locals: HashMap::new(),
            functions: HashMap::new(),
            instructions: Vec::new(),
            calls: Vec::new(),
            func_idx: HashMap::new(),
            curr_func: 0,
            start_ip: 0,
        }
    }

    fn add_instruction(&mut self, instruction: Instruction) -> usize {
        self.instructions.push(instruction);
        self.instructions.len() - 1
    }

    fn add_local(&mut self, name: String) -> usize {
        self.locals.insert(name, self.num_locals);
        self.num_locals += 1;
        return self.num_locals - 1;
    }

    fn get_local(&self, name: &String) -> Option<&usize> {
        self.locals.get(name)
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(lit) => match lit {
                Lit::Int(i) => {
                    self.add_instruction(Instruction::Const(Value::Int(*i)));
                }
                Lit::Char(c) => {
                    self.add_instruction(Instruction::Const(Value::Char(*c)));
                }
                Lit::Bool(b) => {
                    if *b {
                        self.add_instruction(Instruction::True);
                    } else {
                        self.add_instruction(Instruction::False);
                    }
                }
                Lit::Str(s) => {
                    self.add_instruction(Instruction::Const(Value::Str(s.clone())));
                }
                Lit::List(l) => {
                    for expr in l {
                        self.compile_expr(expr);
                    }
                    self.add_instruction(Instruction::List(l.len()));
                }
            },
            Expr::Variable(name) => {
                let local = self.get_local(name);
                if local.is_some() {
                    let local = *local.unwrap();
                    self.add_instruction(Instruction::GetLocal(local));
                } else {
                    panic!("Unknown variable: {}", name);
                }
            }
            Expr::Neg(e) => {
                self.compile_expr(e);
                self.add_instruction(Instruction::Neg);
            }
            Expr::Not(e) => {
                self.compile_expr(e);
                self.add_instruction(Instruction::Not);
            }
            Expr::Arith(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    ArithOp::Add => {
                        self.add_instruction(Instruction::Add);
                    }
                    ArithOp::Sub => {
                        self.add_instruction(Instruction::Sub);
                    }
                    ArithOp::Mul => {
                        self.add_instruction(Instruction::Mul);
                    }
                    ArithOp::Div => {
                        self.add_instruction(Instruction::Div);
                    }
                    ArithOp::Mod => {
                        self.add_instruction(Instruction::Mod);
                    }
                }
            }
            Expr::BoolOp(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    BoolOp::And => {
                        self.add_instruction(Instruction::And);
                    }
                    BoolOp::Or => {
                        self.add_instruction(Instruction::Or);
                    }
                }
            }
            Expr::CmpOp(op, e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                match op {
                    CmpOp::Eq => {
                        self.add_instruction(Instruction::Equal);
                    }
                    CmpOp::Ne => {
                        self.add_instruction(Instruction::NotEqual);
                    }
                    CmpOp::Lt => {
                        self.add_instruction(Instruction::Less);
                    }
                    CmpOp::Le => {
                        self.add_instruction(Instruction::LessEqual);
                    }
                    CmpOp::Gt => {
                        self.add_instruction(Instruction::Greater);
                    }
                    CmpOp::Ge => {
                        self.add_instruction(Instruction::GreaterEqual);
                    }
                }
            }
            Expr::Call(name, args) => {
                if name == "print" {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.add_instruction(Instruction::Print);
                } else if name == "append" {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.add_instruction(Instruction::Append);
                } else {
                    let func = self.func_idx.get(name);
                    if func.is_some() {
                        let func = *func.unwrap();
                        for arg in args {
                            self.compile_expr(arg);
                        }
                        let instr_len = self.instructions.len();
                        self.add_instruction(Instruction::Call(0, args.len()));
                        self.calls[func].push(self.instructions.len() - 1);
                    } else {
                        panic!("Unknown function: {}", name);
                    }
                }
            }
            Expr::Subscr(e1, e2) => {
                self.compile_expr(e1);
                self.compile_expr(e2);
                self.add_instruction(Instruction::Index);
            }
            Expr::Assign(lhs, rhs) => {
                self.compile_expr(rhs);
                match &**lhs {
                    Expr::Variable(name) => {
                        let local = self.get_local(&name);
                        if local.is_some() {
                            let local = *local.unwrap();
                            self.add_instruction(Instruction::Assign(local));
                        } else {
                            panic!("Unknown variable: {}", name);
                        }
                    }
                    Expr::Subscr(e1, e2) => {
                        self.compile_expr(e1);
                        self.compile_expr(e2);
                        self.add_instruction(Instruction::Store);
                    }
                    _ => panic!("Invalid assignment"),
                }
            }
            Expr::Make(ty, size) => {
                match ty {
                    Type::List(ty) => {
                        // create a list of the given size and populate it with the default value
                        let default = match **ty {
                            Type::Int => Value::Int(0),
                            Type::Char => Value::Char(0),
                            Type::Bool => Value::Bool(false),
                            Type::Str => Value::Str(Vec::new()),
                            Type::List(_) => panic!("Cannot create a list of lists"),
                            Type::Chan(_) => panic!("Cannot create a list of channels"),
                            Type::Void => panic!("Cannot create a list of void"),
                            Type::Any => panic!("Cannot create a list of any"),
                        };
                        if *size > 0 {
                            self.add_instruction(Instruction::MakeList(*size, default));
                        } else {
                            self.add_instruction(Instruction::Const(Value::List(Vec::new())));
                        }
                    }
                    Type::Chan(_) => {
                        self.add_instruction(Instruction::MakeChan);
                    }
                    _ => panic!("Cannot make a non-list"),
                }
            }
            Expr::ChanRead(e) => {
                let chan = self.get_local(e).clone();
                if let Some(chan) = chan {
                    let chan = *chan;
                    self.add_instruction(Instruction::GetLocal(chan));
                    self.add_instruction(Instruction::ChanRead);
                } else {
                    panic!("Unknown channel: {}", e);
                }
            }
        }
    }

    fn incr_for_loop(&mut self, idx: usize, start: usize) {
        self.add_instruction(Instruction::GetLocal(idx));
        self.add_instruction(Instruction::Const(Value::Int(1)));
        self.add_instruction(Instruction::Add);
        self.add_instruction(Instruction::Assign(idx));
        self.add_instruction(Instruction::Jump(start + 1));
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr);
                self.add_instruction(Instruction::Return);
            }
            Stmt::IfElse(cond, then, otherwise) => {
                self.compile_expr(cond);
                let jump = self.add_instruction(Instruction::JumpIfFalse(0));
                for stmt in then {
                    self.compile_stmt(stmt);
                }
                let jump2 = self.add_instruction(Instruction::Jump(0));
                self.instructions[jump] = Instruction::JumpIfFalse(self.instructions.len());
                if otherwise.is_some() {
                    let otherwise = otherwise.as_ref().unwrap();
                    for stmt in otherwise {
                        self.compile_stmt(stmt);
                    }
                }
                self.instructions[jump2] = Instruction::Jump(self.instructions.len());
            }
            Stmt::While(cond, body) => {
                let start = self.instructions.len();
                self.compile_expr(cond);
                let jump = self.add_instruction(Instruction::JumpIfFalse(0));
                let mut breaks = Vec::<usize>::new();
                for stmt in body {
                    if stmt == &Stmt::Break {
                        breaks.push(self.add_instruction(Instruction::Jump(start)));
                    } else if stmt == &Stmt::Continue {
                        self.add_instruction(Instruction::Jump(start + 1));
                    } else {
                        self.compile_stmt(stmt);
                    }
                }
                self.add_instruction(Instruction::Jump(start));
                self.instructions[jump] = Instruction::JumpIfFalse(self.instructions.len());
                for break_ in breaks {
                    self.instructions[break_] = Instruction::Jump(self.instructions.len());
                }
            }
            Stmt::For(var, expr, body) => {
                self.compile_expr(expr);
                let arr_var = self.add_local(String::from("<arr>"));
                self.add_instruction(Instruction::DefLocal(arr_var));
                self.add_instruction(Instruction::GetLocal(arr_var));
                self.add_instruction(Instruction::Length);
                let len_var = self.add_local(String::from("<len>"));
                self.add_instruction(Instruction::DefLocal(len_var));
                let idx_var = self.add_local(String::from("<idx>"));
                self.add_instruction(Instruction::Const(Value::Int(0)));
                self.add_instruction(Instruction::DefLocal(idx_var));
                let start = self.instructions.len() - 1;
                self.add_instruction(Instruction::GetLocal(idx_var));
                self.add_instruction(Instruction::GetLocal(len_var));
                self.add_instruction(Instruction::Less);
                let jump = self.add_instruction(Instruction::JumpIfFalse(0));
                let var_var = self.add_local(var.clone());
                self.add_instruction(Instruction::GetLocal(arr_var));
                self.add_instruction(Instruction::GetLocal(idx_var));
                self.add_instruction(Instruction::Index);
                self.add_instruction(Instruction::DefLocal(var_var));
                let mut breaks = Vec::<usize>::new();
                for stmt in body {
                    if *stmt == Stmt::Break {
                        breaks.push(self.add_instruction(Instruction::Jump(start)));
                    } else if *stmt == Stmt::Continue {
                        self.incr_for_loop(arr_var, start);
                    } else {
                        self.compile_stmt(stmt);
                    }
                }
                self.incr_for_loop(idx_var, start);
                self.add_instruction(Instruction::Jump(start));
                let len = self.instructions.len();
                self.instructions[jump] = Instruction::JumpIfFalse(len);
                for break_ in breaks {
                    self.instructions[break_] = Instruction::Jump(len);
                }
            }
            Stmt::Decl(name, expr) => {
                let n = self.add_local(name.clone());
                self.compile_expr(expr);
                self.add_instruction(Instruction::DefLocal(n));
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
            Stmt::Coroutine(expr) => {
                self.add_instruction(Instruction::Coroutine);
                self.compile_expr(expr);
            }
            Stmt::ChanWrite(chan_name, expr) => {
                self.compile_expr(expr);
                let chan = self.get_local(chan_name).clone();
                if let Some(chan) = chan {
                    let chan = *chan;
                    self.add_instruction(Instruction::GetLocal(chan));
                    self.add_instruction(Instruction::ChanWrite);
                } else {
                    panic!("Unknown channel {}", chan_name);
                }
            }
        }
    }

    fn compile_fn(&mut self, name: String, args: &[String], body: &[Stmt]) {
        self.functions.insert(name, self.instructions.len());
        for arg in args {
            self.add_local(arg.to_string());
        }
        for stmt in body {
            self.compile_stmt(stmt);
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        let fns = &program.funcs;
        let mut i = 0;
        for func in fns {
            self.func_idx.insert(func.name.clone(), i);
            println!("{} is given index {}", func.name, i);
            i += 1;
        }
        self.calls = (0..fns.len() + 1).map(|_| Vec::new()).collect();
        let mut func_ip = Vec::<usize>::new();
        for func in fns {
            func_ip.push(self.instructions.len());
            self.locals.clear();
            self.num_locals = 0;
            self.curr_func = self.func_idx[&func.name];
            let name = func.name.clone();
            let args = &func
                .args
                .iter()
                .map(|arg| arg.name.clone())
                .collect::<Vec<_>>();
            let body = &func.body;
            self.compile_fn(name, args, body);
            self.add_instruction(Instruction::Return);
        }
        self.locals.clear();
        self.num_locals = 0;
        self.start_ip = self.instructions.len();
        self.compile_fn("main".to_string(), &[], &program.main);
        self.add_instruction(Instruction::Return);
        // patch calls
        for i in 0..fns.len() {
            for ip in self.calls[i].iter() {
                if let Instruction::Call(_, argc) = self.instructions[*ip] {
                    self.instructions[*ip] = Instruction::Call(func_ip[i], argc);
                } else {
                    panic!("Expected call instruction");
                }
            }
        }
        println!("Instructions: {:#?}", self.instructions);
    }

    pub fn execute(&self) {
        let mut vm = VM::new(&self.instructions, self.start_ip, 0);
        vm.run();
    }
}

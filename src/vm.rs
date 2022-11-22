use crate::code::*;
use std::thread;
use std::time::Duration;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}};

static GLOBAL_THREAD_COUNT: AtomicUsize = AtomicUsize::new(0);


pub struct VM<'a> {
    stack: Vec<Value>,
    instructions: &'a Vec<Instruction>,
    locals: Vec<Vec<Value>>,
    prev_ips: Vec<usize>,
    call_depth: usize,
    about_to_spawn: bool,
    coroutine_depth: usize,
    ip: usize,
}

impl<'a> VM<'a> {
    pub fn new(instructions: &'a Vec<Instruction>, start_ip: usize, depth: usize) -> VM {
        VM {
            stack: Vec::new(),
            instructions,
            locals: Vec::new(),
            prev_ips: Vec::new(),
            about_to_spawn: false,
            coroutine_depth: depth,
            call_depth: 0,
            ip: start_ip,
        }
    }

    pub fn run(&mut self) {
        self.locals.push(Vec::new());
        loop {
            let instruction = self.instructions[self.ip].clone();
            // println!("{:?}", instruction);
            // println!("stack: {:?}", self.stack);
            match instruction {
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::Add => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(b + a));
                        }
                        _ => {
                            panic!("Invalid types for add");
                        }
                    }
                }
                Instruction::Sub => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(b - a));
                        }
                        _ => {
                            panic!("Invalid types for sub");
                        }
                    }
                }
                Instruction::Mul => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(b * a));
                        }
                        _ => {
                            panic!("Invalid types for mul");
                        }
                    }
                }
                Instruction::Div => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(b / a));
                        }
                        _ => {
                            panic!("Invalid types for div");
                        }
                    }
                }
                Instruction::Mod => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(b % a));
                        }
                        _ => {
                            panic!("Invalid types for mod");
                        }
                    }
                }
                Instruction::Neg => {
                    let a = self.stack.pop().unwrap();
                    match a {
                        Value::Int(a) => {
                            self.stack.push(Value::Int(-a));
                        }
                        _ => {
                            panic!("Invalid types for neg");
                        }
                    }
                }
                Instruction::Const(val) => {
                    self.stack.push(val);
                }
                Instruction::False => {
                    self.stack.push(Value::Bool(false));
                }
                Instruction::True => {
                    self.stack.push(Value::Bool(true));
                }
                Instruction::Not => {
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Bool(b) => {
                            self.stack.push(Value::Bool(!b));
                        }
                        _ => {
                            panic!("Invalid type for not");
                        }
                    }
                }
                Instruction::Equal => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        _ => {
                            panic!("Invalid types for equal");
                        }
                    }
                }
                Instruction::NotEqual => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::Str(a), Value::Str(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        _ => {
                            panic!("Invalid types for not equal");
                        }
                    }
                }
                Instruction::Less => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(b < a));
                        }
                        _ => {
                            panic!("Invalid types for less than");
                        }
                    }
                }
                Instruction::Greater => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(b > a));
                        }
                        _ => {
                            panic!("Invalid types for greater than");
                        }
                    }
                }
                Instruction::LessEqual => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(b <= a));
                        }
                        _ => {
                            panic!("Invalid types for less than or equal");
                        }
                    }
                }
                Instruction::GreaterEqual => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(b >= a));
                        }
                        _ => {
                            panic!("Invalid types for greater than or equal");
                        }
                    }
                }
                Instruction::And => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a && b));
                        }
                        _ => {
                            panic!("Invalid types for and");
                        }
                    }
                }
                Instruction::Or => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a || b));
                        }
                        _ => {
                            panic!("Invalid types for or");
                        }
                    }
                }
                Instruction::GetLocal(i) => {
                    self.stack.push(self.locals[self.call_depth][i].clone());
                }
                Instruction::DefLocal(i) => {
                    if i == self.locals[self.call_depth].len() {
                        self.locals[self.call_depth].push(self.stack.pop().unwrap());
                    } else {
                        self.locals[self.call_depth][i] = self.stack.pop().unwrap();
                    }
                }
                Instruction::Call(funcip, args_c) => {
                    if self.about_to_spawn {
                        let instructions = self.instructions.clone();
                        let depth = self.coroutine_depth + 1;
                        let mut locals: Vec<Vec<Value>> = Vec::with_capacity(1);
                        locals.push(Vec::new());
                        for _ in 0..args_c {
                            locals[0].push(self.stack.pop().unwrap());
                        }
                        locals[0].reverse();
                        GLOBAL_THREAD_COUNT.fetch_add(1, Ordering::SeqCst);
                        thread::spawn(move || {
                            let mut vm = VM::new(&instructions, funcip, depth);
                            vm.locals = locals;
                            vm.run();
                        });
                        self.about_to_spawn = false;
                    } else {
                        println!("{}", self.call_depth);
                        self.ip = funcip;
                        self.locals.push(Vec::new());
                        for _ in 0..args_c {
                            self.locals[self.call_depth + 1].push(self.stack.pop().unwrap());
                        }
                        self.locals[self.call_depth + 1].reverse();
                        self.call_depth += 1;
                        continue;
                    }
                }
                Instruction::ReCall(funcip, args_c) => {
                    self.prev_ips.push(self.ip);
                    self.ip = funcip;
                    self.locals[self.call_depth].clear();
                    for _ in 0..args_c {
                        if let Some(val) = self.stack.pop() {
                            self.locals[self.call_depth].push(val);
                        } else {
                            panic!("Stack is empty");
                        }
                    }
                    continue;
                }
                Instruction::Return => {
                    if self.coroutine_depth == 0 {
                        let ip = self.prev_ips.pop();
                        if let Some(ip) = ip {
                            self.call_depth -= 1;
                            self.locals.pop();
                            self.ip = ip;
                            continue;
                        } else {
                            break;
                        }
                    } else {
                        GLOBAL_THREAD_COUNT.fetch_sub(1, Ordering::SeqCst);
                        break;
                    }
                }
                Instruction::ReReturn => {
                    let ip = self.prev_ips.pop();
                    if let Some(ip) = ip {
                        self.ip = ip;
                    } else {
                        break;
                    }
                }
                Instruction::Jump(i) => {
                    self.ip = i;
                    continue;
                }
                Instruction::JumpIfFalse(i) => {
                    let cond = self.stack.pop().unwrap();
                    match cond {
                        Value::Bool(false) => {
                            self.ip = i;
                            continue;
                        }
                        _ => {}
                    }
                }
                Instruction::Assign(local_idx) => {
                    let val = self.stack.pop().unwrap();
                    self.locals[self.call_depth][local_idx] = val;
                }
                Instruction::Print => {
                    let val = self.stack.pop().unwrap();
                    println!("{}", val);
                }
                Instruction::Store => {
                    let idx = self.stack.pop().unwrap();
                    let arr = self.stack.pop().unwrap();
                    let val = self.stack.pop().unwrap();
                    match (idx, arr, val) {
                        (Value::Int(idx), Value::List(mut arr), val) => {
                            arr[idx as usize] = val;
                        }
                        _ => {
                            panic!("Invalid types for store");
                        }
                    }
                }
                Instruction::List(size) => {
                    let mut list = Vec::new();
                    for _ in 0..size {
                        list.push(self.stack.pop().unwrap());
                    }
                    list.reverse();
                    self.stack.push(Value::List(list));
                }
                Instruction::Index => {
                    let idx = self.stack.pop().unwrap();
                    let arr = self.stack.pop().unwrap();
                    match (idx, arr) {
                        (Value::Int(idx), Value::List(arr)) => {
                            self.stack.push(arr[idx as usize].clone());
                        }
                        _ => {
                            panic!("Invalid types for index");
                        }
                    }
                }
                Instruction::Length => {
                    let arr = self.stack.pop().unwrap();
                    match arr {
                        Value::List(arr) => {
                            self.stack.push(Value::Int(arr.len() as i64));
                        }
                        _ => {
                            panic!("Invalid types for len");
                        }
                    }
                }
                Instruction::Append => {
                    let val = self.stack.pop().unwrap();
                    let arr = self.stack.pop().unwrap();
                    match (val, arr) {
                        (val, Value::List(mut arr)) => {
                            arr.push(val);
                            self.stack.push(Value::List(arr));
                        }
                        _ => {
                            panic!("Invalid types for append");
                        }
                    }
                }
                Instruction::Coroutine => {
                    self.about_to_spawn = true;
                }
                Instruction::MakeChan => {
                    let chan = Arc::new(Mutex::new(VecDeque::<Value>::new()));
                    self.stack.push(Value::Chan(chan));
                }
                Instruction::ChanWrite => {
                    let chan = self.stack.pop().unwrap();
                    let val = self.stack.pop().unwrap();
                    match chan {
                        Value::Chan(chan) => {
                            chan.lock().unwrap().push_back(val);
                        }
                        _ => {
                            panic!("Invalid types for chan write");
                        }
                    }
                }
                Instruction::ChanRead => {
                    let chan = self.stack.pop().unwrap();
                    match chan {
                        Value::Chan(chan) => {
                            while chan.lock().unwrap().is_empty() {
                                thread::sleep(Duration::from_millis(10));
                            }
                            self.stack.push(chan.lock().unwrap().pop_front().unwrap());
                        }
                        _ => {
                            panic!("Invalid types for chan read");
                        }
                    }
                }
            }
            self.ip += 1;
        }
        loop {
            if GLOBAL_THREAD_COUNT.load(Ordering::SeqCst) == 0 {
                break;
            }
            thread::sleep(Duration::from_millis(5));
        }
    }
}

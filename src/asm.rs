use crate::semant::FuncEnv;
use crate::syntax::*;
use rand::distributions::Alphanumeric;
use rand::prelude::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

const DEBUG: bool = true;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Reg64 {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl std::fmt::Display for Reg64 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Reg64::Rax => write!(f, "rax"),
            Reg64::Rcx => write!(f, "rcx"),
            Reg64::Rdx => write!(f, "rdx"),
            Reg64::Rbx => write!(f, "rbx"),
            Reg64::Rsp => write!(f, "rsp"),
            Reg64::Rbp => write!(f, "rbp"),
            Reg64::Rsi => write!(f, "rsi"),
            Reg64::Rdi => write!(f, "rdi"),
            Reg64::R8 => write!(f, "r8"),
            Reg64::R9 => write!(f, "r9"),
            Reg64::R10 => write!(f, "r10"),
            Reg64::R11 => write!(f, "r11"),
            Reg64::R12 => write!(f, "r12"),
            Reg64::R13 => write!(f, "r13"),
            Reg64::R14 => write!(f, "r14"),
            Reg64::R15 => write!(f, "r15"),
        }
    }
}

impl From<u8> for Reg64 {
    fn from(n: u8) -> Self {
        match n {
            0 => Reg64::Rax,
            1 => Reg64::Rcx,
            2 => Reg64::Rdx,
            3 => Reg64::Rbx,
            4 => Reg64::Rsp,
            5 => Reg64::Rbp,
            6 => Reg64::Rsi,
            7 => Reg64::Rdi,
            8 => Reg64::R8,
            9 => Reg64::R9,
            10 => Reg64::R10,
            11 => Reg64::R11,
            12 => Reg64::R12,
            13 => Reg64::R13,
            14 => Reg64::R14,
            15 => Reg64::R15,
            _ => panic!("Invalid register number"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Reg32 {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
    R8d,
    R9d,
    R10d,
    R11d,
    R12d,
    R13d,
    R14d,
    R15d,
}

impl std::fmt::Display for Reg32 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Reg32::Eax => write!(f, "eax"),
            Reg32::Ecx => write!(f, "ecx"),
            Reg32::Edx => write!(f, "edx"),
            Reg32::Ebx => write!(f, "ebx"),
            Reg32::Esp => write!(f, "esp"),
            Reg32::Ebp => write!(f, "ebp"),
            Reg32::Esi => write!(f, "esi"),
            Reg32::Edi => write!(f, "edi"),
            Reg32::R8d => write!(f, "r8d"),
            Reg32::R9d => write!(f, "r9d"),
            Reg32::R10d => write!(f, "r10d"),
            Reg32::R11d => write!(f, "r11d"),
            Reg32::R12d => write!(f, "r12d"),
            Reg32::R13d => write!(f, "r13d"),
            Reg32::R14d => write!(f, "r14d"),
            Reg32::R15d => write!(f, "r15d"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Reg16 {
    Ax,
    Cx,
    Dx,
    Bx,
    Sp,
    Bp,
    Si,
    Di,
    R8w,
    R9w,
    R10w,
    R11w,
    R12w,
    R13w,
    R14w,
    R15w,
}

impl std::fmt::Display for Reg16 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Reg16::Ax => write!(f, "ax"),
            Reg16::Cx => write!(f, "cx"),
            Reg16::Dx => write!(f, "dx"),
            Reg16::Bx => write!(f, "bx"),
            Reg16::Sp => write!(f, "sp"),
            Reg16::Bp => write!(f, "bp"),
            Reg16::Si => write!(f, "si"),
            Reg16::Di => write!(f, "di"),
            Reg16::R8w => write!(f, "r8w"),
            Reg16::R9w => write!(f, "r9w"),
            Reg16::R10w => write!(f, "r10w"),
            Reg16::R11w => write!(f, "r11w"),
            Reg16::R12w => write!(f, "r12w"),
            Reg16::R13w => write!(f, "r13w"),
            Reg16::R14w => write!(f, "r14w"),
            Reg16::R15w => write!(f, "r15w"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Reg8 {
    Al,
    Cl,
    Dl,
    Bl,
    Spl,
    Bpl,
    Sil,
    Dil,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}

impl std::fmt::Display for Reg8 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Reg8::Al => write!(f, "al"),
            Reg8::Cl => write!(f, "cl"),
            Reg8::Dl => write!(f, "dl"),
            Reg8::Bl => write!(f, "bl"),
            Reg8::Spl => write!(f, "spl"),
            Reg8::Bpl => write!(f, "bpl"),
            Reg8::Sil => write!(f, "sil"),
            Reg8::Dil => write!(f, "dil"),
            Reg8::R8b => write!(f, "r8b"),
            Reg8::R9b => write!(f, "r9b"),
            Reg8::R10b => write!(f, "r10b"),
            Reg8::R11b => write!(f, "r11b"),
            Reg8::R12b => write!(f, "r12b"),
            Reg8::R13b => write!(f, "r13b"),
            Reg8::R14b => write!(f, "r14b"),
            Reg8::R15b => write!(f, "r15b"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum MemType {
    R64,
    R32,
    R16,
    R8,
    Xmm,
    Stack,
    Data,
    Imm,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mem {
    R64(Reg64),
    R32(Reg32),
    R16(Reg16),
    R8(Reg8),
    Xmm(u8),
    Stack(u32),
    Data(u64),
    Imm(u64),
}

fn is_reg_mem(mem: &Mem) -> bool {
    match mem {
        Mem::R64(_) => true,
        Mem::R32(_) => true,
        Mem::R16(_) => true,
        Mem::R8(_) => true,
        Mem::Xmm(_) => true,
        Mem::Stack(_) => false,
        Mem::Data(_) => false,
        Mem::Imm(_) => false,
    }
}

fn is_stack_mem(mem: &Mem) -> bool {
    match mem {
        Mem::R64(_) => false,
        Mem::R32(_) => false,
        Mem::R16(_) => false,
        Mem::R8(_) => false,
        Mem::Stack(_) => true,
        Mem::Xmm(_) => false,
        Mem::Data(_) => false,
        Mem::Imm(_) => false,
    }
}

fn is_data_mem(mem: &Mem) -> bool {
    match mem {
        Mem::R64(_) => false,
        Mem::R32(_) => false,
        Mem::R16(_) => false,
        Mem::R8(_) => false,
        Mem::Stack(_) => false,
        Mem::Xmm(_) => false,
        Mem::Data(_) => true,
        Mem::Imm(_) => false,
    }
}

fn mem_type(mem: &Mem) -> MemType {
    match mem {
        Mem::R64(_) => MemType::R64,
        Mem::R32(_) => MemType::R32,
        Mem::R16(_) => MemType::R16,
        Mem::R8(_) => MemType::R8,
        Mem::Stack(_) => MemType::Stack,
        Mem::Xmm(_) => MemType::Xmm,
        Mem::Data(_) => MemType::Data,
        Mem::Imm(_) => MemType::Imm,
    }
}

impl std::fmt::Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Mem::R64(r) => write!(f, "{}", r),
            Mem::R32(r) => write!(f, "{}", r),
            Mem::R16(r) => write!(f, "{}", r),
            Mem::R8(r) => write!(f, "{}", r),
            Mem::Stack(i) => {
                let i = *i;
                write!(f, "[rbp - {}]", i)
            }
            Mem::Xmm(i) => write!(f, "xmm{}", i),
            Mem::Data(i) => write!(f, "[d{}]", i),
            Mem::Imm(i) => write!(f, "{}", i),
        }
    }
}

pub struct Asm {
    code: Vec<String>,
    stack: Vec<Mem>,
    loops: Vec<(String, String)>,
    loop_count: usize,
    used_regs: [bool; 23], // first 16 are rxx, next 7 are xmmx
    vars: HashMap<String, Mem>,
    varty: HashMap<String, Type>,
    strings: Vec<Vec<u8>>,
    curr_fn_name: String,
    curr_fn_start: u32,
    stack_size: u32,
    var_offset: u32,
    return_label: String,
    return_stack_space: u32,
    functy: FuncEnv,
}

//  rdx, rcx, r8, r9, r10, r11
const SCRATCH_REGS: [u8; 5] = [2, 1, 8, 9, 10];

// arg regs - rdi, rsi, rdx, rcx, r8, r9
const ARG_REGS: [u8; 6] = [7, 6, 2, 1, 8, 9];

impl Asm {
    pub fn new(functy: FuncEnv) -> Self {
        Self {
            code: Vec::new(),
            stack: Vec::new(),
            loops: Vec::new(),
            loop_count: 0,
            used_regs: [false; 23],
            vars: HashMap::new(),
            varty: HashMap::new(),
            strings: Vec::new(),
            curr_fn_name: String::new(),
            curr_fn_start: 0,
            stack_size: 0,
            var_offset: 0,
            return_stack_space: 0,
            return_label: String::new(),
            functy,
        }
    }

    fn push(&mut self, v: Mem) {
        if is_reg_mem(&v) {
            let r = match v {
                Mem::R64(r) => r as u8,
                Mem::R32(r) => r as u8,
                Mem::R16(r) => r as u8,
                Mem::R8(r) => r as u8,
                Mem::Xmm(r) => r + 16,
                Mem::Stack(_) => unreachable!(),
                Mem::Data(_) => unreachable!(),
                Mem::Imm(_) => unreachable!(),
            };
            self.used_regs[r as usize] = true;
        }
        self.stack.push(v);
    }

    fn tyerr(&self, a: Type, b: &[Type]) -> ! {
        panic!("type error: {:?} != {:?}", a, b);
    }

    fn rand_label(&self) -> String {
        let rng = rand::thread_rng();
        let s = rng
            .sample_iter(&Alphanumeric)
            .take(7)
            .map(char::from)
            .collect::<String>();
        format!(".{}_{}", self.curr_fn_name, s)
    }

    fn pop(&mut self) -> Mem {
        let m = self.stack.pop().unwrap();
        println!("pop {}", m);
        match m {
            Mem::R64(r) => {
                self.used_regs[r as usize] = false;
            }
            Mem::R32(r) => {
                self.used_regs[r as usize] = false;
            }
            Mem::R16(r) => {
                self.used_regs[r as usize] = false;
            }
            Mem::R8(r) => {
                self.used_regs[r as usize] = false;
            }
            Mem::Xmm(r) => {
                self.used_regs[r as usize + 16] = false;
            }
            Mem::Stack(_) => {}
            Mem::Data(_) => {}
            Mem::Imm(_) => {}
        }
        m
    }

    fn pop2(&mut self) -> (Mem, Mem) {
        let a = self.pop();
        let b = self.pop();
        (a, b)
    }

    fn peek(&self, i: usize) -> Mem {
        self.stack[self.stack.len() - 1 - i].clone()
    }

    fn unused_reg(&mut self) -> Reg64 {
        // only scratch regs
        for i in 0..SCRATCH_REGS.len() {
            let r = SCRATCH_REGS[i];
            if !self.used_regs[r as usize] {
                self.used_regs[r as usize] = true;
                let reg : Reg64 = (r as u8).into();
                println!("using reg {}", reg);
                return unsafe {
                    // safe because we only use the first 16
                    std::mem::transmute(r)
                };
            }
        }
        for v in self.stack.iter() {
            println!("{:?}", v);
        }
        panic!("no unused registers");
    }

    fn append(&mut self, s: &str, indent: usize) {
        let s = format!("{}{}", "    ".repeat(indent), s);
        if DEBUG {
            println!("{}", s.as_str());
        }
        self.code.push(s);
    }

    fn header(&mut self) -> Vec<String> {
        let mut header = Vec::new();
        header.push("default rel\n".to_string());
        header.push("section .data\n".to_string());
        for (i, s) in self.strings.iter().enumerate() {
            // comma separated list of bytes
            let s = s
                .iter()
                .map(|b| b.to_string())
                .collect::<Vec<String>>()
                .join(",");
            header.push(format!("d{}: db {},0", i, s));
        }

        header.push("\nsection .text\n".to_string());
        header.push("global _start\n".to_string());

        // prints
        header.push("extern iprint".to_string());
        header.push("extern fprint".to_string());
        header.push("extern bprint".to_string());
        header.push("extern cprint".to_string());
        header.push("extern sprint".to_string());
        header.push("extern lprint".to_string());

        // arith ops
        header.push("extern ifadd".to_string());
        header.push("extern ffadd".to_string());
        header.push("extern ifsub".to_string());
        header.push("extern ffsub".to_string());
        header.push("extern ifmul".to_string());
        header.push("extern ffmul".to_string());
        header.push("extern iidiv".to_string());
        header.push("extern ifdiv".to_string());
        header.push("extern fidiv".to_string());
        header.push("extern ffdiv".to_string());

        // cmp ops
        header.push("extern ifcmp".to_string());
        header.push("extern ffcmp".to_string());

        // string
        header.push("extern newstr".to_string());
        header.push("extern strcatt".to_string());

        // list
        header.push("extern new_list".to_string());
        header.push("extern list_append".to_string());
        header.push("extern list_get".to_string());
        header.push("extern list_set".to_string());
        header.push("extern list_set_all".to_string());
        header.push("extern list_len".to_string());

        // cast
        header.push("extern cast_int_to_char".to_string());
        header.push("extern cast_int_to_double".to_string());
        header.push("extern cast_double_to_int".to_string());
        header.push("extern cast_char_to_int".to_string());

        return header;
    }

    fn swap_reg(&mut self, a: Reg64, b: Reg64) {
        self.append(&format!("push {}", a), 1);
        self.append(&format!("mov {}, {}", a, b), 1);
        self.append(&format!("pop {}", b), 1);
    }

    fn unused_xmm(&mut self) -> Mem {
        for i in 0..8 {
            if !self.used_regs[i + 16] {
                self.used_regs[i + 16] = true;
                return Mem::Xmm(i as u8);
            }
        }
        panic!("no unused xmm registers");
    }

    fn free_all_regs(&mut self) {
        for i in 0..16 {
            self.used_regs[i] = false
        }
    }

    fn get_arg_mem(&mut self, i: usize) -> Mem {
        if i < 6 {
            let v = unsafe { std::mem::transmute(ARG_REGS[i]) };
            Mem::R64(v)
        } else {
            let i = (i - 6) * 8;
            Mem::Stack(i as u32)
        }
    }

    fn get_f_arg_mem(&mut self, i: usize) -> Mem {
        if i < 8 {
            Mem::Xmm(i as u8)
        } else {
            let i = (i - 8) * 8;
            Mem::Stack(i as u32)
        }
    }

    fn get_result_mem(&mut self, ty: Type) -> Mem {
        match ty {
            Type::Int => Mem::R64(Reg64::Rax),
            Type::Double => Mem::Xmm(0),
            Type::Bool => Mem::R8(Reg8::Al),
            Type::Char => Mem::R64(Reg64::Rax),
            Type::Str => Mem::R64(Reg64::Rax),
            Type::Void => Mem::R64(Reg64::Rax),
            _ => Mem::R64(Reg64::Rax),
        }
    }

    // choose the appropriate instruction
    fn write_mem(&mut self, m: &Mem, v: &Mem) {
        self.emit_mov(m, &v);
    }

    fn free_reg(&mut self, r: u8) {
        self.used_regs[r as usize] = false;
    }

    fn free_mem(&mut self, m: &Mem) {
        match m {
            Mem::R64(r) => self.free_reg(*r as u8),
            Mem::Xmm(r) => self.free_reg(*r as u8 + 16),
            _ => {}
        }
    }

    fn emit_call(&mut self, name: &str, args: &[Mem], ret_ty: Type, argtypes: &[Type]) {
        let mut i = 0;
        for a in args {
            let m = if argtypes[i] == Type::Double {
                self.get_f_arg_mem(i)
            } else {
                self.get_arg_mem(i)
            };
            self.write_mem(&m, &a.clone());
            self.free_mem(&a);
            i += 1;
        }
        self.append(&format!("call {}", name), 1);
        let m = self.get_result_mem(ret_ty.clone());
        self.push(m);
        if ret_ty == Type::Double {
            self.used_regs[16] = true;
        } else {
            self.used_regs[0] = true;
        }
    }

    fn emit_mov(&mut self, dest: &Mem, src: &Mem) {
        println!("mov {} <- {}", dest, src);
        if dest == src {
            return;
        }
        // between general purpose registers and xmm registers
        let dest_ty = mem_type(dest);
        let src_ty = mem_type(src);
        if dest_ty == MemType::Xmm || src_ty == MemType::Xmm {
            self.append(&format!("movq {}, {}", dest, src), 1);
        } else if dest_ty == MemType::R64 && src_ty == MemType::R64 {
            self.append(&format!("mov {}, {}", dest, src), 1);
        } else if dest_ty == MemType::R8 && src_ty == MemType::R8 {
            self.append(&format!("mov {}, {}", dest, src), 1);
        } else if dest_ty == MemType::R64 && src_ty == MemType::R8 {
            self.append(&format!("movzx {}, {}", dest, src), 1);
        } else if dest_ty == MemType::R8 && src_ty == MemType::R64 {
            self.append(&format!("mov {}, {}", dest, src), 1);
        } else if dest_ty == MemType::R64 && src_ty == MemType::Stack
            || dest_ty == MemType::Stack && src_ty == MemType::R64
        {
            self.append(&format!("mov {}, {}", dest, src), 1);
        } else if dest_ty == MemType::Stack && src_ty == MemType::R8 {
            let reg = self.unused_reg();
            self.emit_mov(&Mem::R64(reg), src);
            self.emit_mov(dest, &Mem::R64(reg));
        } else if src_ty == MemType::Data {
            self.append(&format!("lea {}, {}", dest, src), 1);
        } else if src_ty == MemType::Stack && dest_ty == MemType::Stack {
            let reg = self.unused_reg();
            self.emit_mov(&Mem::R64(reg), src);
            self.emit_mov(dest, &Mem::R64(reg));
        } else if src_ty == MemType::Imm {
            self.append(&format!("mov {}, {}", dest, src), 1);
        } else {
            panic!("unhandled mov: {:?} {:?}", dest, src);
        }
        self.free_mem(src);
    }

    // compile expr to x86_64 intel syntax assembly
    fn compile_expr(&mut self, expr: &Expr) {
        if DEBUG {
            println!("compile_expr: {:?}", expr);
        }
        match expr {
            Expr::Literal { lit, ty } => {
                let reg = self.unused_reg();
                match lit {
                    Lit::Int(i) => {
                        self.append(&format!("mov {}, {}", reg, i), 1);
                        let v = Mem::R64(reg);
                        self.push(v);
                    }
                    Lit::Double(f) => {
                        let f = f.to_bits();
                        self.append(&format!("mov {}, {}", reg, f), 1);
                        let v = Mem::R64(reg);
                        self.push(v);
                    }
                    Lit::Bool(b) => {
                        self.append(&format!("mov {}, {}", reg, b), 1);
                        let v = Mem::R64(reg);
                        self.push(v);
                    }
                    Lit::Char(c) => {
                        self.append(&format!("mov {}, {}", reg, c), 1);
                        let v = Mem::R64(reg);
                        self.push(v);
                    }
                    Lit::Str(s) => {
                        self.strings.push(s.clone());
                        let v = Mem::Data((self.strings.len() - 1) as u64);
                        self.emit_call("newstr", &[v], Type::Str, &[Type::Str]);
                    }
                    _ => unimplemented!("this literal is not implemented yet"),
                }
            }
            Expr::Variable { name, ty } => {
                let var = self.vars.get(name).unwrap();
                self.push(var.clone());
            }
            Expr::Arith {
                op,
                left,
                right,
                ty,
            } => {
                self.compile_expr(&(*left).1);
                self.compile_expr(&(*right).1);
                let (a, b) = (self.peek(0), self.peek(1));
                let func = match op.clone() {
                    ArithOp::Add => "add",
                    ArithOp::Mul => "imul",
                    ArithOp::Sub => "sub",
                    ArithOp::Div => "idiv",
                    ArithOp::Mod => "mod",
                };
                let leftty = left.1.get_type();
                let rightty = right.1.get_type();
                if leftty == Type::Str && rightty == Type::Str {
                    // string concat
                    self.emit_call("strcatt", &[a, b], Type::Str, &[Type::Str, Type::Str]);
                } else {
                    match op.clone() {
                        ArithOp::Add | ArithOp::Sub | ArithOp::Mul => {
                            if leftty == Type::Int && rightty == Type::Int {
                                // both are regs
                                if is_reg_mem(&a) && is_reg_mem(&b) {
                                    self.append(&format!("{} {}, {}", func, a, b), 1);
                                    self.pop2();
                                    self.push(a);
                                } else if is_stack_mem(&a) && is_stack_mem(&b) {
                                    // both are stack
                                    let reg = self.unused_reg();
                                    self.append(&format!("mov {}, {}", reg, a), 1);
                                    self.append(&format!("{} {}, {}", func, reg, b), 1);
                                    self.pop2();
                                    self.push(Mem::R64(reg));
                                } else {
                                    // one is reg, one is stack
                                    let (a, b) = if is_reg_mem(&a) { (a, b) } else { (b, a) };
                                    self.append(&format!("{} {}, {}", func, a, b), 1);
                                    self.pop2();
                                    self.push(a);
                                }
                            } else {
                                // call the appropriate function
                                if leftty == Type::Double && rightty == Type::Double {
                                    // call ffadd
                                    self.pop2();
                                    self.emit_call(
                                        "ffadd",
                                        &[a, b],
                                        Type::Double,
                                        &[Type::Double, Type::Double],
                                    );
                                } else {
                                    let (a, b) = if leftty == Type::Double {
                                        (a, b)
                                    } else {
                                        (b, a)
                                    };
                                    self.pop2();
                                    self.emit_call(
                                        format!("if{}", func).as_str(),
                                        &[a, b],
                                        Type::Double,
                                        &[Type::Int, Type::Double],
                                    );
                                }
                            }
                        }
                        ArithOp::Div => {
                            if leftty == Type::Int && rightty == Type::Int {
                                // call idiv
                                self.pop2();
                                self.emit_call("idiv", &[a, b], Type::Int, &[Type::Int, Type::Int]);
                            } else {
                                if leftty == Type::Double && rightty == Type::Double {
                                    // call ffdiv
                                    self.pop2();
                                    self.emit_call(
                                        "ffdiv",
                                        &[a, b],
                                        Type::Double,
                                        &[Type::Double, Type::Double],
                                    );
                                } else if leftty == Type::Double {
                                    self.pop2();
                                    self.emit_call(
                                        "fidiv",
                                        &[b, a],
                                        Type::Double,
                                        &[Type::Int, Type::Double],
                                    );
                                } else {
                                    self.pop2();
                                    self.emit_call(
                                        "ifdiv",
                                        &[a, b],
                                        Type::Double,
                                        &[Type::Int, Type::Double],
                                    );
                                }
                            }
                        }
                        ArithOp::Mod => {
                            if leftty == Type::Int && rightty == Type::Int {
                                self.append(&format!("mov {}, {}", Reg64::Rax, a), 1);
                                self.append(&format!("xor {}, {}", Reg64::Rdx, Reg64::Rdx), 1);
                                self.append(&format!("idiv {}", b), 1);
                                let v = Mem::R64(Reg64::Rdx);
                                self.pop2();
                                self.push(v);
                            }
                        }
                    }
                }
            }
            Expr::BoolOp {
                op,
                left,
                right,
                ty,
            } => {
                self.compile_expr(&(*left).1);
                self.compile_expr(&(*right).1);
                let (a, b) = (self.peek(0), self.peek(1));
                let func = match op.clone() {
                    BoolOp::And => "and",
                    BoolOp::Or => "or",
                };
                let leftty = left.1.get_type();
                let rightty = right.1.get_type();
                if leftty == Type::Bool && rightty == Type::Bool {
                    if is_reg_mem(&a) && is_reg_mem(&b) {
                        // both are regs
                        self.append(&format!("{} {}, {}", func, a, b), 1);
                        self.pop2();
                        self.push(a);
                    } else if is_stack_mem(&a) && is_stack_mem(&b) {
                        // both are stack
                        let reg = self.unused_reg();
                        self.append(&format!("mov {}, {}", reg, a), 1);
                        self.append(&format!("{} {}, {}", func, reg, b), 1);
                        self.pop2();
                        self.push(Mem::R64(reg));
                    } else {
                        // one is reg, one is mem
                        let (stv, regv) = if is_reg_mem(&a) { (b, a) } else { (a, b) };
                        self.append(&format!("{} {}, {}", func, regv, stv), 1);
                        self.pop2();
                        self.push(regv);
                    }
                } else {
                    self.tyerr(leftty, &[rightty]);
                }
            }
            Expr::CmpOp {
                op,
                left,
                right,
                ty,
            } => {
                self.compile_expr(&(*left).1);
                self.compile_expr(&(*right).1);
                let (a, b) = (self.peek(0), self.peek(1));
                let res = self.get_result_mem(Type::Bool);
                let leftty = left.1.get_type();
                let rightty = right.1.get_type();
                if (leftty == Type::Int && rightty == Type::Int)
                    || (leftty == Type::Char && rightty == Type::Char)
                {
                    let (b, a) = if is_stack_mem(&a) && is_stack_mem(&b) {
                        let reg = self.unused_reg();
                        let v = Mem::R64(reg);
                        self.emit_mov(&v, &b);
                        (v, a)
                    } else if is_stack_mem(&b) {
                        let reg = self.unused_reg();
                        let v = Mem::R64(reg);
                        self.emit_mov(&v, &a);
                        (b, v)
                    } else {
                        (b, a)
                    };
                    self.append(&format!("cmp {}, {}", b, a), 1);
                    let j1 = match op.clone() {
                        CmpOp::Eq => "je",
                        CmpOp::Ne => "jne",
                        CmpOp::Lt => "jl",
                        CmpOp::Gt => "jg",
                        CmpOp::Le => "jle",
                        CmpOp::Ge => "jge",
                    };
                    let label = self.rand_label();
                    self.append(&format!("{} {}", j1, label), 1);
                    self.append(&format!("mov {}, 0", res), 1);
                    let label2 = self.rand_label();
                    self.append(&format!("jmp {}", label2), 1);
                    self.append(&format!("{}:", label), 1);
                    self.append(&format!("mov {}, 1", res), 1);
                    self.append(&format!("{}:", label2), 1);
                } else if leftty == Type::Double || rightty == Type::Double {
                    let (a, b) = if leftty == Type::Double {
                        (b, a)
                    } else {
                        (a, b)
                    };
                    // 1: a > b
                    // 0: a == b
                    // -1: a < b
                    self.pop2();
                    if leftty == Type::Double && rightty == Type::Double {
                        self.emit_call("ffcmp", &[a, b], Type::Int, &[Type::Double, Type::Double]);
                    } else {
                        self.emit_call("ifcmp", &[a, b], Type::Int, &[Type::Int, Type::Double]);
                    }
                    self.append(&format!("cmp {}, 0", res), 1);
                    let res = self.get_result_mem(Type::Bool);
                    match op.clone() {
                        CmpOp::Eq => {
                            self.append(&format!("sete {}", res), 1);
                        }
                        CmpOp::Ne => {
                            self.append(&format!("setne {}", res), 1);
                        }
                        CmpOp::Lt => {
                            self.append(&format!("setl {}", res), 1);
                        }
                        CmpOp::Gt => {
                            self.append(&format!("setg {}", res), 1);
                        }
                        CmpOp::Le => {
                            self.append(&format!("setle {}", res), 1);
                        }
                        CmpOp::Ge => {
                            self.append(&format!("setge {}", res), 1);
                        }
                    }
                } else {
                    self.tyerr(leftty, &[rightty]);
                }
                self.push(res);
            }
            Expr::Call { name, args, ty } => {
                if name == "print" {
                    // call respective print for each Mem
                    for arg in args {
                        self.compile_expr(&(*arg).1);
                        let v = self.pop();
                        let argty  = arg.1.get_type();
                        if argty == Type::Int {
                            self.emit_call("iprint", &[v], Type::Void, &[Type::Int]);
                        } else if argty == Type::Double {
                            self.emit_call("fprint", &[v], Type::Void, &[Type::Double]);
                        } else if argty == Type::Bool {
                            self.emit_call("bprint", &[v], Type::Void, &[Type::Int]);
                        } else if argty == Type::Char {
                            self.emit_call("cprint", &[v], Type::Void, &[Type::Int]);
                        } else if argty == Type::Str {
                            self.emit_call("sprint", &[v], Type::Void, &[Type::Int]);
                        } else {
                            self.tyerr(
                                argty,
                                &[Type::Int, Type::Double, Type::Bool, Type::Char, Type::Str],
                            );
                        }
                        self.free_all_regs();
                    }
                } else if name == "len" {
                    if args.len() != 1 {
                        println!("len takes exactly 1 argument");
                        std::process::exit(1);
                    }
                    self.compile_expr(&args[0].1);
                    let v = self.pop();
                    let argty = args[0].1.get_type();
                    if let Type::List(_) = argty {
                    } else {
                        self.tyerr(argty, &[Type::List(Box::new(Type::Int))]);
                    }
                    self.emit_call(
                        "list_len",
                        &[v],
                        Type::Int,
                        &[Type::List(Box::new(Type::Int))],
                    );
                } else if name == "append" {
                    if args.len() != 2 {
                        println!("append takes exactly 2 argument");
                        std::process::exit(1);
                    }
                    self.compile_expr(&args[0].1);
                    let list = self.peek(0);
                    // void list_append(uint64_t list, void *val)
                    let argty = args[0].1.get_type();
                    if let Type::List(_) = argty {
                    } else {
                        self.tyerr(argty, &[Type::List(Box::new(Type::Int))]);
                    }
                    self.compile_expr(&args[1].1);
                    let val = self.peek(0);
                    let valty = argty.clone();
                    let arg1_reg = self.get_arg_mem(0);
                    let arg2_reg = if valty == Type::Double {
                        self.get_f_arg_mem(0)
                    } else {
                        self.get_arg_mem(1)
                    };
                    self.emit_mov(&arg1_reg, &list);
                    self.append(&format!("lea {}, {}", arg2_reg, val), 1);
                    self.append("call list_append", 1);
                } else {
                    let args = args
                        .iter()
                        .map(|arg| {
                            self.compile_expr(&(*arg).1);
                            self.peek(0)
                        })
                        .collect::<Vec<_>>();
                    let (argstys, retty) = self.functy.get(name).unwrap().clone();
                    let name = &format!("func_{}", name);
                    // pop args
                    for _ in 0..args.len() {
                        self.pop();
                    }
                    self.emit_call(name, &args, retty.clone(), argstys.as_slice());
                }
            }
            Expr::Assign { left, right } => {
                match left.1.clone() {
                    Expr::Variable { name, ty } => {
                        self.compile_expr(&(*right).1);
                        let v = self.pop();
                        let var = self.vars.get(&name).unwrap().clone();
                        let mem = var;
                        // both stack
                        if is_stack_mem(&mem) && is_stack_mem(&v) {
                            let reg = self.unused_reg();
                            let temp = Mem::R64(reg);
                            self.emit_mov(&temp, &v);
                            self.emit_mov(&mem, &mem);
                        } else {
                            self.emit_mov(&mem, &v);
                        }
                        self.push(mem);
                    }
                    Expr::Subscr { expr, index, ty } => {
                        self.compile_expr(&(*expr).1);
                        let v = self.peek(0);
                        self.compile_expr(&(*index).1);
                        let idx = self.peek(0);
                        let name_str = if let Expr::Variable { name, ty } = expr.1.clone() {
                            name
                        } else {
                            panic!("invalid lhs in assign")
                        };
                        let ls = self.vars.get(&name_str).unwrap().clone();
                        let arg1_reg = self.get_arg_mem(0);
                        let arg2_reg = self.get_arg_mem(1);
                        let arg3_reg = self.get_arg_mem(2);
                        // void list_set(uint64_t list, uint64_t index, void *val)
                        self.emit_mov(&arg1_reg, &ls);
                        self.emit_mov(&arg2_reg, &idx);
                        self.append(&format!("lea {}, {}", arg3_reg, v), 1);
                        self.append(&format!("call list_set"), 1);
                    }
                    _ => {
                        panic!("invalid lhs in assign")
                    }
                }
            }
            Expr::Make { ty, expr } => {
                let elemsize = match ty.clone() {
                    Type::List(t) | Type::Chan(t) => match *t {
                        Type::Bool => 1,
                        Type::Char => 1,
                        _ => 8,
                    },
                    _ => {
                        self.tyerr(
                            ty.clone(),
                            &[
                                Type::List(Box::new(Type::Any)),
                                Type::Chan(Box::new(Type::Any)),
                            ],
                        );
                    }
                };

                let lenreg = match expr {
                    Some(ex) => {
                        self.compile_expr(&(*ex).1);
                        let v = self.peek(0);
                        let reg = self.unused_reg();
                        let temp = Mem::R64(reg);
                        self.emit_mov(&temp, &v);
                        let v = temp.clone();
                        v
                    }
                    None => Mem::Imm(0),
                };

                match *ty {
                    Type::List(_) => {
                        let reg = self.unused_reg();
                        let temp = Mem::R64(reg);
                        self.append(&format!("mov {}, {}", temp, elemsize), 1);
                        let elemsize = temp;

                        self.emit_call(
                            "new_list",
                            &[elemsize, lenreg],
                            ty.clone(),
                            &[Type::Int, Type::Int],
                        );
                    }
                    _ => {
                        self.tyerr(ty.clone(), &[Type::List(Box::new(Type::Any))]);
                    }
                }
            }
            Expr::Subscr { expr, index, ty } => {
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                self.compile_expr(&(*index).1);
                let idx = self.pop();
                let vty = expr.1.get_type();
                let idxty = index.1.get_type();
                let mut is_string = false;
                let valty = match vty.clone() {
                    Type::List(ty) => ty,
                    Type::Str => {
                        is_string = true;
                        Box::new(Type::Char)
                    }
                    _ => {
                        self.tyerr(vty, &[Type::List(Box::new(Type::Any))]);
                    }
                };
                let arg1_reg = self.get_arg_mem(0);
                let arg2_reg = self.get_arg_mem(1);
                let arg3_reg = self.get_arg_mem(2);
                // void list_get(uint64_t list, uint64_t index, void *val)
                self.emit_mov(&arg1_reg, &v);
                self.emit_mov(&arg2_reg, &idx);
                self.append(&format!("lea {}, {}", arg3_reg, idx), 1);
                if is_string {
                    self.append(&format!("call str_get"), 1);
                } else {
                    self.append(&format!("call list_get"), 1);
                }
                // extract Mem from arg3_reg
                let reg = self.unused_reg();
                let val = Mem::R64(reg);
                self.append(&format!("mov {}, {}", val, arg3_reg), 1);
                self.push(val);
            }
            Expr::Cast { expr, ty } => {
                //
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                let vty = expr.1.get_type();
                let ty = ty.clone();
                match (vty.clone(), ty.clone()) {
                    (Type::Int, Type::Char) => {
                        // call cast_int_to_char
                        self.emit_call("cast_int_to_char", &[v], Type::Char, &[Type::Int]);
                    }
                    (Type::Char, Type::Int) => {
                        // call cast_char_to_int
                        self.emit_call("cast_char_to_int", &[v], Type::Int, &[Type::Char]);
                    }
                    (Type::Int, Type::Double) => {
                        // call cast_int_to_double
                        self.emit_call("cast_int_to_double", &[v], Type::Double, &[Type::Int]);
                    }
                    _ => unimplemented!("cast {:?} to {:?}", vty, ty),
                }
            }
            _ => {
                unimplemented!("{:?}", expr)
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(&(*expr).1);
                self.free_all_regs();
            }
            Stmt::Decl(name, expr) => {
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                let m = Mem::Stack(self.var_offset);
                self.write_mem(&m, &v.clone());
                let val = Mem::Stack(self.var_offset);
                self.vars.insert(name.clone(), val);
                self.var_offset += 8;
            }
            Stmt::IfElse(expr, stmts, else_stmts) => {
                let label_else = self.rand_label();
                let label_end = self.rand_label();
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                self.append(&format!("cmp {}, 0", v), 1);
                if !else_stmts.is_none() {
                    self.append(&format!("je {}", label_else), 1);
                }
                for stmt in stmts {
                    self.compile_stmt(&(*stmt).1);
                }
                if !else_stmts.is_none() {
                    self.append(&format!("jmp {}", label_end), 1);
                    self.append(&format!("{}:", label_else), 0);
                }
                for els in else_stmts {
                    for stmt in els {
                        self.compile_stmt(&(*stmt).1);
                    }
                }
                self.append(&format!("{}:", label_end), 0);
            }
            Stmt::While(expr, stmts) => {
                let label_start = self.rand_label();
                let label_end = self.rand_label();
                self.append(&format!("{}:", label_start.clone()), 0);
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                self.append(&format!("cmp {}, 0", v), 1);
                self.append(&format!("je {}", label_end.clone()), 1);
                self.loops.push((label_start.clone(), label_end.clone()));
                for stmt in stmts {
                    self.compile_stmt(&(*stmt).1);
                }
                self.append(&format!("jmp {}", label_start), 1);
                self.append(&format!("{}:", label_end), 0);
            }
            Stmt::Return(expr) => {
                self.compile_expr(&(*expr).1);
                let v = self.pop();
                self.emit_mov(&Mem::Stack(self.return_stack_space), &v);
                self.append(&format!("jmp {}", self.return_label), 1);
            }
            Stmt::Comment => {}
            _ => {
                unimplemented!();
            }
        }
    }

    fn compile_fn(&mut self, func: &Func) {
        let name = if func.name == "main" {
            "_start".to_string()
        } else {
            format!("func_{}", func.name)
        };
        self.curr_fn_name = func.name.clone();
        self.append(&format!("{}:", name), 0);
        self.stack.clear();
        self.free_all_regs();
        self.append("push rbp", 1);
        self.append("mov rbp, rsp", 1);
        let stack_init_idx = self.code.len(); // space for local variables

        self.var_offset = if func.args.len() > 6 {
            (func.args.len() as u32 - 6) * 8
        } else {
            0
        };
        self.return_label = self.rand_label();
        self.var_offset += 8;
        self.return_stack_space = self.var_offset;
        self.var_offset += 8;

        // add arguments to stack
        for i in 0..func.args.len() {
            let arg = self.get_arg_mem(i);
            let mem = Mem::Stack(self.var_offset);
            self.emit_mov(&mem, &arg);
            self.vars.insert(
                func.args[i].name.clone(),
                mem.clone(),
            );
            self.var_offset += 8;
        }

        for stmt in &func.body {
            self.compile_stmt(&(*stmt).1);
        }
        self.append(&format!("{}:", self.return_label), 0);
        self.code
            .insert(stack_init_idx, format!("    sub rsp, {}", self.var_offset));
        self.emit_mov(&Mem::R64(Reg64::Rax), &Mem::Stack(self.return_stack_space));
        if func.name == "main" {
            self.append("mov rax, 60", 1);
            self.append("mov rdi, 0", 1);
            self.append("syscall", 1);
        } else {
            self.append("mov rsp, rbp", 1);
            self.append("pop rbp", 1);
            self.append("ret", 1);
        }
    }

    pub fn compile(&mut self, prog: &Program) {
        for func in &prog.funcs {
            self.compile_fn(func);
        }
        // write code to file exe.asm
        let mut file = File::create("exe.asm").unwrap();
        for line in self.header() {
            file.write_all(line.as_bytes()).unwrap();
            file.write_all(b"\n").unwrap();
        }
        for line in &self.code {
            file.write_all(line.as_bytes()).unwrap();
            file.write_all(b"\n").unwrap();
        }
    }
}

use std::collections::HashMap;

use crate::syntax::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType, PointerType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

#[derive(Debug, Clone, PartialEq)]
enum TypeType {
    Ptr,
    Int,
}

#[derive(Debug, Clone)]
enum LLVMVal<'val> {
    Ptr(PointerValue<'val>),
    Int(IntValue<'val>),
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub funcs: Vec<&'a Func>,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    curr_func: usize,
    variables: HashMap<String, (PointerValue<'ctx>, Type)>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx>
where
    'a: 'ctx,
{
    pub fn new(context: &'ctx Context, funcs: Vec<&'a Func>) -> Self {
        Self {
            context,
            funcs,
            curr_func: 0,
            variables: HashMap::new(),
            module: context.create_module("sahl"),
            builder: context.create_builder(),
        }
    }

    fn get_type_type(&self, t: &Type) -> TypeType {
        match t {
            Type::Str => TypeType::Ptr,
            Type::List(_) => TypeType::Ptr,
            _ => TypeType::Int,
        }
    }

    fn type_conv(&self, t: &'a Type) -> BasicMetadataTypeEnum<'ctx> {
        match self.get_type_type(t) {
            TypeType::Ptr => self.get_ptr_type(t).into(),
            TypeType::Int => self.get_int_type(t).into(),
        }
    }
    fn get_int_type(&self, t: &'a Type) -> IntType<'ctx> {
        match t {
            Type::Int => self.context.i64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i8_type().into(),
            _ => panic!("Unsupported type"),
        }
    }

    fn get_ptr_type(&self, t: &'a Type) -> PointerType<'ctx> {
        match t {
            Type::Str => self.context.i8_type().ptr_type(AddressSpace::Generic),
            Type::List(ty) => self.type_conv(ty).into_pointer_type(),
            _ => panic!("Unsupported type"),
        }
    }

    fn lookup_var(&self, name: &str) -> (PointerValue<'ctx>, Type) {
        self.variables.get(name).unwrap().clone()
    }

    fn insert_var(&mut self, name: &str, val: (PointerValue<'ctx>, Type)) {
        self.variables.insert(name.to_string(), val);
    }

    fn arithop(&self, op: &ArithOp, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> LLVMVal<'ctx> {
        let val = match op {
            ArithOp::Add => self.builder.build_int_add(lhs, rhs, "addtmp"),
            ArithOp::Sub => self.builder.build_int_sub(lhs, rhs, "subtmp"),
            ArithOp::Mul => self.builder.build_int_mul(lhs, rhs, "multmp"),
            ArithOp::Div => self.builder.build_int_signed_div(lhs, rhs, "divtmp"),
            ArithOp::Mod => self.builder.build_int_signed_rem(lhs, rhs, "modtmp"),
        };
        LLVMVal::Int(val)
    }

    fn cmpop(&self, op: &CmpOp, val1: IntValue<'ctx>, val2: IntValue<'ctx>) -> LLVMVal<'ctx> {
        let val = match op {
            CmpOp::Eq => self
                .builder
                .build_int_compare(IntPredicate::EQ, val1, val2, "eqtmp"),
            CmpOp::Ne => self
                .builder
                .build_int_compare(IntPredicate::NE, val1, val2, "netmp"),
            CmpOp::Lt => self
                .builder
                .build_int_compare(IntPredicate::SLT, val1, val2, "lttmp"),
            CmpOp::Gt => self
                .builder
                .build_int_compare(IntPredicate::SGT, val1, val2, "gttmp"),
            CmpOp::Le => self
                .builder
                .build_int_compare(IntPredicate::SLE, val1, val2, "letmp"),
            CmpOp::Ge => self
                .builder
                .build_int_compare(IntPredicate::SGE, val1, val2, "getmp"),
        };
        LLVMVal::Int(val)
    }

    fn compile_expr(&self, expr: &'a Expr) -> (LLVMVal<'ctx>, Type) {
        match expr {
            Expr::Literal(lit) => match lit {
                Lit::Int(i) => (
                    LLVMVal::Int(self.context.i64_type().const_int(*i as u64, false)),
                    Type::Int,
                ),
                Lit::Bool(b) => (
                    LLVMVal::Int(self.context.bool_type().const_int(*b as u64, false)),
                    Type::Bool,
                ),
                Lit::Str(s) => {
                    let str_type = self.context.i8_type().array_type(s.len() as u32);
                    let str_value = self.context.const_string(s, false);
                    let global = self.module.add_global(str_type, None, "str");
                    global.set_initializer(&str_value);
                    (LLVMVal::Ptr(global.as_pointer_value()), Type::Str)
                }
                Lit::Char(c) => (
                    LLVMVal::Int(self.context.i8_type().const_int(*c as u64, false)),
                    Type::Char,
                ),
                Lit::List(l) => {
                    let mut vals = Vec::new();
                    for expr in l {
                        vals.push(self.compile_expr(expr));
                    }
                    match &vals[0] {
                        (LLVMVal::Ptr(_), ty) => {
                            let list_vals = vals
                                .iter()
                                .map(|(val, _)| match val {
                                    LLVMVal::Ptr(ptr) => (*ptr).into(),
                                    _ => panic!("Invalid list"),
                                })
                                .collect::<Vec<BasicValueEnum>>();
                            let typee = self.context.i64_type().array_type(list_vals.len() as u32);
                            let size_val = self
                                .context
                                .i64_type()
                                .const_int(list_vals.len() as u64, false);
                            let list_res = self.builder.build_array_malloc(typee, size_val, "list");
                            if let Ok(list) = list_res {
                                for (i, val) in list_vals.iter().enumerate() {
                                    let index = self.context.i64_type().const_int(i as u64, false);
                                    unsafe {
                                        let ptr = self.builder.build_in_bounds_gep(
                                            list,
                                            &[index],
                                            "list_ptr",
                                        );
                                        self.builder.build_store(ptr, *val);
                                    }
                                }
                                (LLVMVal::Ptr(list), Type::List(Box::new(ty.clone())))
                            } else {
                                panic!("Invalid list");
                            }
                        }
                        (LLVMVal::Int(_), ty) => {
                            let list_vals = vals
                                .iter()
                                .map(|(val, _)| match val {
                                    LLVMVal::Int(int) => (*int).into(),
                                    LLVMVal::Ptr(ptr) => self.builder.build_load(*ptr, "load"),
                                })
                                .collect::<Vec<BasicValueEnum>>();
                            let size_val = self
                                .context
                                .i64_type()
                                .const_int(list_vals.len() as u64, false);
                            let typee = self.context.i64_type();
                            let list_res = self.builder.build_array_malloc(typee, size_val, "list");
                            if let Ok(list) = list_res {
                                for (i, val) in list_vals.iter().enumerate() {
                                    let index = self.context.i64_type().const_int(i as u64, false);
                                    unsafe {
                                        let ptr = self.builder.build_in_bounds_gep(
                                            list,
                                            &[index],
                                            "list_ptr",
                                        );
                                        self.builder.build_store(ptr, *val);
                                    }
                                }
                                (LLVMVal::Ptr(list), Type::List(Box::new(ty.clone())))
                            } else {
                                panic!("Invalid list");
                            }
                        }
                    }
                },
                _ => panic!("Invalid literal"),
            },
            Expr::Variable(v) => {
                let var = self.lookup_var(v);
                (LLVMVal::Ptr(var.0), var.1)
            }
            Expr::Arith(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs);
                let rhs = self.compile_expr(rhs);
                let res = match (lhs, rhs) {
                    ((LLVMVal::Int(lhs), _), (LLVMVal::Int(rhs), _)) => self.arithop(op, lhs, rhs),
                    ((LLVMVal::Ptr(lhs), _), (LLVMVal::Ptr(rhs), _)) => {
                        let lhs = self.builder.build_load(lhs, "lhs").into_int_value();
                        let rhs = self.builder.build_load(rhs, "rhs").into_int_value();
                        self.arithop(op, lhs, rhs)
                    }
                    ((LLVMVal::Ptr(lhs), _), (LLVMVal::Int(rhs), _)) => {
                        let lhs = self.builder.build_load(lhs, "lhs").into_int_value();
                        self.arithop(op, lhs, rhs)
                    }
                    ((LLVMVal::Int(lhs), _), (LLVMVal::Ptr(rhs), _)) => {
                        let rhs = self.builder.build_load(rhs, "lhs").into_int_value();
                        self.arithop(op, lhs, rhs)
                    }
                };
                (res, Type::Int)
            }
            Expr::Neg(expr) => {
                let expr = self.compile_expr(expr);
                match expr {
                    (LLVMVal::Int(expr), _) => (
                        LLVMVal::Int(self.builder.build_int_neg(expr, "negtmp")),
                        Type::Int,
                    ),
                    _ => {
                        panic!("Invalid types for neg op");
                    }
                }
            }
            Expr::BoolOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs);
                let rhs = self.compile_expr(rhs);
                let res = match (lhs, rhs) {
                    ((LLVMVal::Int(lhs), _), (LLVMVal::Int(rhs), _)) => LLVMVal::Int(match op {
                        BoolOp::And => self.builder.build_and(lhs, rhs, "andtmp"),
                        BoolOp::Or => self.builder.build_or(lhs, rhs, "ortmp"),
                    }),
                    _ => {
                        panic!("Invalid types for bool op");
                    }
                };
                (res, Type::Bool)
            }
            Expr::Call(name, args) => {
                let callee = match name.as_str() {
                    "print" => {
                        if args.len() != 1 {
                            panic!("Invalid number of arguments for print");
                        }
                        match args[0] {
                            Expr::Literal(Lit::Str(_)) => {
                                self.module.get_function("print_str").unwrap()
                            }
                            Expr::Literal(Lit::Char(_)) => {
                                self.module.get_function("print_char").unwrap()
                            }
                            Expr::Literal(Lit::Int(_)) => {
                                self.module.get_function("print_int").unwrap()
                            }
                            Expr::Variable(_) => self.module.get_function("print_int").unwrap(),
                            _ => panic!("Invalid argument type for print"),
                        }
                    }
                    _ => self.module.get_function(name).unwrap(),
                };
                let args = if name == "print" {
                    let arg = &args[0];
                    let (arg, ty) = self.compile_expr(&arg);
                    match ty {
                        Type::Str => {
                            let arg = match arg {
                                LLVMVal::Ptr(ptr) => ptr,
                                _ => panic!("Invalid argument type for print"),
                            };
                            let arg = self.builder.build_load(arg, "load");
                            let arg = self.builder.build_bitcast(
                                arg,
                                self.context.i8_type().ptr_type(AddressSpace::Generic),
                                "cast",
                            );
                            vec![arg.into()]
                        }
                        Type::Char => {
                            let arg = match arg {
                                LLVMVal::Int(int) => int,
                                LLVMVal::Ptr(ptr) => {
                                    self.builder.build_load(ptr, "load").into_int_value()
                                }
                            };
                            vec![arg.into()]
                        }
                        Type::Int => {
                            let arg = match arg {
                                LLVMVal::Int(int) => int,
                                LLVMVal::Ptr(ptr) => {
                                    self.builder.build_load(ptr, "load").into_int_value()
                                }
                            };
                            vec![arg.into()]
                        }
                        _ => panic!("Invalid argument type for print"),
                    }
                } else {
                    args.iter()
                        .map(|arg| {
                            let (val, ty) = self.compile_expr(arg);
                            match ty {
                                Type::Int => {
                                    let val = match val {
                                        LLVMVal::Int(int) => int,
                                        LLVMVal::Ptr(ptr) => {
                                            self.builder.build_load(ptr, "load").into_int_value()
                                        }
                                    };
                                    val.into()
                                }
                                Type::Char => {
                                    let val = match val {
                                        LLVMVal::Int(int) => int,
                                        LLVMVal::Ptr(ptr) => {
                                            self.builder.build_load(ptr, "load").into_int_value()
                                        }
                                    };
                                    val.into()
                                }
                                Type::Str => {
                                    let val = match val {
                                        LLVMVal::Ptr(ptr) => ptr,
                                        _ => panic!("Invalid argument type"),
                                    };
                                    let val = self.builder.build_load(val, "load");
                                    let val = self.builder.build_bitcast(
                                        val,
                                        self.context.i8_type().ptr_type(AddressSpace::Generic),
                                        "cast",
                                    );
                                    val.into()
                                }
                                Type::List(_) => {
                                    let val = match val {
                                        LLVMVal::Ptr(ptr) => ptr,
                                        _ => panic!("Invalid argument type"),
                                    };
                                    val.into()
                                }
                                Type::Bool => {
                                    let val = match val {
                                        LLVMVal::Int(int) => int,
                                        LLVMVal::Ptr(ptr) => {
                                            self.builder.build_load(ptr, "load").into_int_value()
                                        }
                                    };
                                    val.into()
                                }
                                _ => panic!("Invalid argument type"),
                            }
                        })
                        .collect::<Vec<_>>()
                };
                let call = self.builder.build_call(callee, &args, "calltmp");
                let res = if let Some(val) = call.try_as_basic_value().left() {
                    if self.get_type_type(&self.funcs[self.curr_func].retty) == TypeType::Int {
                        LLVMVal::Int(val.into_int_value())
                    } else {
                        LLVMVal::Ptr(val.into_pointer_value())
                    }
                } else {
                    LLVMVal::Int(self.context.i64_type().const_zero())
                };
                (res, self.funcs[self.curr_func].retty.clone())
            }
            Expr::Assign(lhs, rhs) => {
                let val = self.compile_expr(rhs);
                let var = self.compile_expr(lhs);
                match (var.0, val.0) {
                    (LLVMVal::Ptr(var), LLVMVal::Int(v)) => {
                        self.builder.build_store(var, v);
                        (LLVMVal::Int(v), val.1)
                    }
                    (LLVMVal::Ptr(var), LLVMVal::Ptr(v)) => {
                        self.builder.build_store(var, v);
                        (LLVMVal::Ptr(v), val.1)
                    }
                    _ => {
                        panic!("Invalid types for assign");
                    }
                }
            }
            Expr::Not(expr) => {
                let expr = self.compile_expr(expr);
                match expr.0 {
                    LLVMVal::Int(val) => {
                        let val = self.builder.build_not(val, "not");
                        (LLVMVal::Int(val), Type::Bool)
                    }
                    LLVMVal::Ptr(val) => {
                        let val = self.builder.build_load(val, "load");
                        let val = self.builder.build_not(val.into_int_value(), "not");
                        (LLVMVal::Int(val), Type::Bool)
                    }
                }
            }
            Expr::CmpOp(op, ex1, ex2) => {
                let ex1 = self.compile_expr(ex1);
                let ex2 = self.compile_expr(ex2);
                let res = match (ex1.0, ex2.0) {
                    (LLVMVal::Int(val1), LLVMVal::Int(val2)) => self.cmpop(op, val1, val2),
                    (LLVMVal::Ptr(val1), LLVMVal::Int(val2)) => {
                        let val1 = self.builder.build_load(val1, "loadtmp").into_int_value();
                        self.cmpop(op, val1, val2)
                    }
                    (LLVMVal::Int(val1), LLVMVal::Ptr(val2)) => {
                        let val2 = self.builder.build_load(val2, "loadtmp").into_int_value();
                        self.cmpop(op, val1, val2)
                    }
                    (LLVMVal::Ptr(val1), LLVMVal::Ptr(val2)) => {
                        let val1 = self.builder.build_load(val1, "loadtmp").into_int_value();
                        let val2 = self.builder.build_load(val2, "loadtmp").into_int_value();
                        self.cmpop(op, val1, val2)
                    }
                };
                (res, Type::Bool)
            }
            Expr::Subscr(expr, index) => {
                let expr = self.compile_expr(expr);
                let index = self.compile_expr(index);
                unsafe {
                    match (expr.0, index.0, expr.1) {
                        (LLVMVal::Ptr(ex), LLVMVal::Int(index), Type::List(ty)) => {
                            let ptr = self.builder.build_gep(ex, &[index.into()], "subscrtmp");
                            (LLVMVal::Ptr(ptr), *ty)
                        }
                        _ => {
                            panic!("Invalid types for subscr");
                        }
                    }
                }
            }
            Expr::Make(ty, expr) => {
                let len = if let Some(len) = expr {
                    let len = self.compile_expr(len);
                    match len.0 {
                        LLVMVal::Int(len) => len,
                        _ => panic!("Invalid type for len"),
                    }
                } else {
                    self.context.i64_type().const_int(0, false)
                };
                let tyty = self.get_type_type(ty);
                let ptr = match tyty {
                    TypeType::Int => {
                        let ptr =
                            self.builder
                                .build_array_alloca(self.get_int_type(ty), len, "make_ptr");
                        self.builder
                            .build_store(ptr, self.context.i64_type().const_int(0, false));
                        ptr
                    }
                    TypeType::Ptr => {
                        let ptr =
                            self.builder
                                .build_array_alloca(self.get_ptr_type(ty), len, "make_ptr");
                        self.builder
                            .build_store(ptr, self.context.i64_type().const_int(0, false));
                        ptr
                    }
                };
                (LLVMVal::Ptr(ptr), Type::List(Box::new(ty.clone())))
            }
            Expr::ChanRead(_) => {
                panic!("unimplemented");
            }
            _ => {
                panic!("unimplemented");
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
            }
            Stmt::Return(expr) => {
                let ret = self.compile_expr(expr);
                match ret.0 {
                    LLVMVal::Int(ret) => {
                        self.builder.build_return(Some(&ret));
                    }
                    LLVMVal::Ptr(ret) => {
                        self.builder.build_return(Some(&ret));
                    }
                };
            }
            Stmt::IfElse(cond, thens, otherwise) => {
                let cond = self.compile_expr(cond);
                let then_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "then");
                let else_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "else");
                let merge_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "merge");
                match cond.0 {
                    LLVMVal::Int(cond) => {
                        self.builder
                            .build_conditional_branch(cond, then_block, else_block);
                    }
                    _ => {
                        panic!("Invalid type for if cond");
                    }
                }
                self.builder.position_at_end(then_block);
                for stmt in thens {
                    self.compile_stmt(stmt);
                }
                if let Some(otherwise) = otherwise {
                    self.builder.build_unconditional_branch(merge_block);
                    self.builder.position_at_end(else_block);
                    for stmt in otherwise {
                        self.compile_stmt(stmt);
                    }
                    self.builder.build_unconditional_branch(merge_block);
                    self.builder.position_at_end(merge_block);
                } else {
                    self.builder.build_unconditional_branch(merge_block);
                    self.builder.position_at_end(merge_block);
                }
            }
            Stmt::While(cond, body) => {
                let cond_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "cond");
                let body_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "body");
                let merge_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "merge");
                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(cond_block);
                let cond = self.compile_expr(cond);
                match cond.0 {
                    LLVMVal::Int(cond) => {
                        self.builder
                            .build_conditional_branch(cond, body_block, merge_block);
                    }
                    _ => {
                        panic!("Invalid type for while cond");
                    }
                }
                self.builder.position_at_end(body_block);
                for stmt in body {
                    self.compile_stmt(stmt);
                }
                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(merge_block);
            }
            Stmt::Decl(var, expr) => {
                let expr = self.compile_expr(expr);
                match expr.0 {
                    LLVMVal::Int(ex) => {
                        let alloca = self.builder.build_alloca(ex.get_type(), var);
                        self.builder.build_store(alloca, ex);
                        self.insert_var(var, (alloca, expr.1));
                    }
                    LLVMVal::Ptr(ex) => {
                        let alloca = self.builder.build_alloca(ex.get_type(), var);
                        self.builder.build_store(alloca, ex);
                        self.insert_var(var, (alloca, expr.1));
                    }
                }
            }
            Stmt::For(var, list, stmts) => {
                let list = self.compile_expr(list);
                let len = match list.0 {
                    LLVMVal::Ptr(list) => self.builder.build_load(list, "len"),
                    _ => {
                        panic!("Invalid type for for list");
                    }
                };
                let cond_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "cond");
                let body_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "body");
                let merge_block = self
                    .context
                    .append_basic_block(self.module.get_last_function().unwrap(), "merge");
                let i = self.builder.build_alloca(self.context.i64_type(), "<i>");
                self.builder
                    .build_store(i, self.context.i64_type().const_int(0, false));
                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(cond_block);
                let i_val = self.builder.build_load(i, "<i>");
                let cond = self.builder.build_int_compare(
                    IntPredicate::ULT,
                    i_val.into_int_value(),
                    len.into_int_value(),
                    "cond",
                );
                self.builder
                    .build_conditional_branch(cond, body_block, merge_block);
                self.builder.position_at_end(body_block);
                let list = match list.0 {
                    LLVMVal::Ptr(list) => self.builder.build_load(list, "list"),
                    _ => {
                        panic!("Invalid type for for list");
                    }
                };
                let el_ptr = unsafe {
                    self.builder.build_gep(
                        list.into_pointer_value(),
                        &[
                            self.context.i64_type().const_int(0, false),
                            i_val.into_int_value(),
                        ],
                        "list_ptr",
                    )
                };
                let el = self.builder.build_load(el_ptr, "el");
                let alloca = self.builder.build_alloca(el.get_type(), var);
                self.builder.build_store(alloca, el);
                for stmt in stmts {
                    self.compile_stmt(stmt);
                }
                let i_val = self.builder.build_load(i, "<i>");
                let i_val = self.builder.build_int_add(
                    i_val.into_int_value(),
                    self.context.i64_type().const_int(1, false),
                    "<i>",
                );
                self.builder.build_store(i, i_val);
                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(merge_block);
            }
            Stmt::Coroutine(_) => {
                panic!("unimplemented")
            }
            Stmt::ChanWrite(_, _) => {
                panic!("unimplemented")
            }
            Stmt::Continue => todo!(),
            Stmt::Comment => todo!(),
            Stmt::Break => todo!(),
        }
    }

    fn compile_func(&mut self, func: &'a Func) {
        let argsty = func
            .args
            .iter()
            .map(|x| self.type_conv(&x.ty))
            .collect::<Vec<_>>();
        let functy = if func.retty == Type::Void {
            self.context.void_type().fn_type(&argsty, false)
        } else {
            match self.get_type_type(&func.retty) {
                TypeType::Ptr => self.get_ptr_type(&func.retty).fn_type(&argsty[..], false),
                TypeType::Int => self.get_int_type(&func.retty).fn_type(&argsty[..], false),
            }
        };
        let function = self.module.add_function(&func.name, functy, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        let mut args = function.get_params().into_iter();
        let mut locals = Vec::new();
        for arg in &func.args {
            let argval = args.next().unwrap();
            let alloca = self.builder.build_alloca(argval.get_type(), &arg.name);
            self.builder.build_store(alloca, argval);
            locals.push(alloca);
        }
        for stmt in &func.body {
            self.compile_stmt(stmt);
        }
        self.builder.build_return(None);
    }

    fn add_builtin_funcs(&self) {
        let print_int = self
            .context
            .void_type()
            .fn_type(&[self.context.i64_type().into()], false);
        self.module.add_function("print_int", print_int, None);
        let print_char = self
            .context
            .void_type()
            .fn_type(&[self.context.i8_type().into()], false);
        self.module.add_function("print_char", print_char, None);
        let print_str = self.context.void_type().fn_type(
            &[self
                .context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .into()],
            false,
        );
        self.module.add_function("print_str", print_str, None);
    }

    pub fn compile(&mut self) {
        self.add_builtin_funcs();
        for i in 0..self.funcs.len() {
            self.compile_func(self.funcs[i]);
        }
    }

    pub fn write(&self, path: &str) {
        self.module.print_to_file(path).unwrap();
    }
}

use crate::syntax::*;
use std::collections::HashMap;

pub enum Error {
    TypeMismatch(Vec<Type>, Vec<Type>),
    ArityMismatch(usize, usize),
    UndefinedVariable(String),
    UndefinedFunction(String),
    CoroutineNotFunction,
    ContinueOutsideLoop,
    BreakOutsideLoop,
    IncorrectLHS(Expr),
    NoReturn,
    NoMain,
    MainArgs,
    MainNotVoid,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(expected, actual) => {
                write!(
                    f,
                    "Type mismatch: expected {:?}, found {:?}",
                    expected, actual
                )
            }
            Error::CoroutineNotFunction => write!(f, "Coroutine is not a function"),
            Error::ArityMismatch(expected, actual) => {
                write!(f, "Arity mismatch: expected {}, found {}", expected, actual)
            }
            Error::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            Error::UndefinedFunction(name) => {
                write!(f, "Undefined function: {}", name)
            }
            Error::ContinueOutsideLoop => {
                write!(f, "Continue statement outside of loop")
            }
            Error::BreakOutsideLoop => {
                write!(f, "Break statement outside of loop")
            }
            Error::IncorrectLHS(expr) => {
                write!(f, "Incorrect LHS: {:?}", expr)
            }
            Error::NoReturn => {
                write!(f, "There is a path in control flow that does not return")
            }
            Error::NoMain => {
                write!(f, "No main function")
            }
            Error::MainArgs => {
                write!(f, "Main function must take no arguments")
            }
            Error::MainNotVoid => {
                write!(f, "Main function must return void")
            }
        }
    }
}

type TypeEnv = Vec<HashMap<String, Type>>;
type FuncEnv = HashMap<String, (Vec<Type>, Type)>;

fn builtin_fn(name: &str, args: &[Type]) -> Result<Type, Error> {
    match name {
        "print" => {
            if args.len() == 1 {
                Ok(Type::Void)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "append" => {
            if args.len() == 2 {
                match &args[0] {
                    Type::List(t) => {
                        let ty = *(t.clone());
                        if ty == args[1] {
                            Ok(Type::List(Box::new(ty)))
                        } else {
                            Err(Error::TypeMismatch(vec![ty], vec![args[1].clone()]))
                        }
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![args[0].clone()],
                    )),
                }
            } else {
                Err(Error::ArityMismatch(2, args.len()))
            }
        }
        "len" => {
            if args.len() == 1 {
                match &args[0] {
                    Type::List(_) => Ok(Type::Int),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![args[0].clone()],
                    )),
                }
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "clear" => {
            Ok(Type::Void)
        }
        "sleep" => {
            if args.len() == 1 {
                Ok(Type::Void)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "rand" => {
            if args.len() == 2 {
                Ok(Type::Int)
            } else {
                Err(Error::ArityMismatch(2, args.len()))
            }
        },
        "randf" => {
            if args.len() == 0 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(0, args.len()))
            }
        },
        _ => Err(Error::UndefinedVariable(name.to_string())),
    }
}

struct Checker<'a> {
    type_env: TypeEnv,
    func_env: &'a FuncEnv,
    loop_depth: usize,
    func_ret_type: Type,
    scope: usize,
}

impl<'a> Checker<'a> {
    fn new(func_env: &'a FuncEnv, retty: Type) -> Self {
        Checker {
            func_env,
            type_env: vec![HashMap::new()],
            func_ret_type: retty,
            loop_depth: 0,
            scope: 1,
        }
    }

    fn find_var(&self, name: &str) -> Result<Type, Error> {
        for i in (0..self.scope).rev() {
            if let Some(ty) = self.type_env[i].get(name) {
                return Ok(ty.clone());
            }
        }
        Err(Error::UndefinedVariable(name.to_string()))
    }

    fn check_expr(&self, expr: &Expr) -> Result<Type, Error> {
        // println!("Checking expr: {:?}", expr);
        match expr {
            Expr::Literal(lit) => match lit {
                Lit::Int(_) => Ok(Type::Int),
                Lit::Bool(_) => Ok(Type::Bool),
                Lit::Char(_) => Ok(Type::Char),
                Lit::Str(_) => Ok(Type::Str),
                Lit::Double(_) => Ok(Type::Double),
                Lit::List(list) => {
                    let type_ = self.check_expr(&list.iter().next().unwrap())?;
                    for elem in list.iter().skip(1) {
                        let ty = self.check_expr(elem)?;
                        if type_ != ty {
                            return Err(Error::TypeMismatch(vec![type_], vec![ty]));
                        }
                    }
                    Ok(Type::List(Box::new(type_)))
                }
            },
            Expr::Variable(name) => self.find_var(name),
            Expr::Assign(lhs, expr) => {
                let ty = self.check_expr(expr)?;
                let lhs = &**lhs;
                let correct_lhs = match lhs {
                    Expr::Variable(_) => true,
                    Expr::Subscr(_, _) => true,
                    _ => false,
                };
                if correct_lhs {
                    let lhs_ty = self.check_expr(lhs)?;
                    if ty == lhs_ty {
                        Ok(ty)
                    } else {
                        Err(Error::TypeMismatch(vec![ty], vec![lhs_ty]))
                    }
                } else {
                    Err(Error::IncorrectLHS(lhs.clone()))
                }
            }
            Expr::Neg(e) => {
                let ty = self.check_expr(e)?;
                if ty == Type::Int {
                    Ok(ty)
                } else {
                    Err(Error::TypeMismatch(vec![Type::Int], vec![ty]))
                }
            }
            Expr::Not(e) => {
                let ty = self.check_expr(e)?;
                if ty == Type::Bool {
                    Ok(ty)
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty]))
                }
            }
            Expr::Arith(op, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if *op == ArithOp::Add && ty1 == Type::Str && ty2 == Type::Str {
                    Ok(Type::Str)
                } else if ty1 == Type::Int && ty2 == Type::Int {
                    Ok(Type::Int)
                } else if ty1 == Type::Double || ty2 == Type::Double {
                    Ok(Type::Double)
                } else {
                    Err(Error::TypeMismatch(vec![Type::Int, Type::Double], vec![ty1, ty2]))
                }
            }
            Expr::BoolOp(_, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1 == Type::Bool && ty2 == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty1, ty2]))
                }
            }
            Expr::CmpOp(_, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1 == ty2 {
                    Ok(Type::Bool)
                } else {
                    Err(Error::TypeMismatch(vec![ty1.clone()], vec![ty1, ty2]))
                }
            }
            Expr::Call(name, args) => {
                let argsty = args
                    .iter()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let res = builtin_fn(name, &argsty);

                match res {
                    Ok(ty) => return Ok(ty),
                    Err(Error::UndefinedVariable(_)) => {} // if builtin function not found, continue
                    Err(e) => return Err(e),
                }

                let (formal_types, ret_type) = self
                    .func_env
                    .get(name)
                    .ok_or(Error::UndefinedFunction(name.clone()))?;

                if argsty.len() != formal_types.len() {
                    return Err(Error::ArityMismatch(formal_types.len(), args.len()));
                }

                for (argty, formal_type) in argsty.iter().zip(formal_types) {
                    if argty != formal_type {
                        return Err(Error::TypeMismatch(
                            vec![argty.clone()],
                            vec![formal_type.clone()],
                        ));
                    }
                }
                Ok(ret_type.clone())
            }
            Expr::Subscr(expr, index) => {
                let ty = self.check_expr(expr)?;
                let index_ty = self.check_expr(index)?;
                match ty {
                    Type::List(ty) => {
                        if index_ty == Type::Int {
                            Ok(*ty.clone())
                        } else {
                            Err(Error::TypeMismatch(vec![Type::Int], vec![index_ty]))
                        }
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![ty],
                    )),
                }
            }
            Expr::ChanRead(chan_name) => {
                let ty = self.find_var(chan_name)?;
                match ty {
                    Type::Chan(ty) => Ok(*ty.clone()),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![ty],
                    )),
                }
            }
            Expr::Make(ty, _) => Ok(ty.clone()),
            Expr::Tuple(exprs) => {
                let tys = exprs
                    .iter()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(tys))
            }
            Expr::Range(ex1, ex2, _) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                match (&ty1, &ty2) {
                    (Type::Int, Type::Int) => Ok(Type::Range),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Int, Type::Int],
                        vec![ty1, ty2],
                    )),
                }
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        // println!("check_stmt: {:?}", stmt);
        match stmt {
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
                Ok(())
            }
            Stmt::IfElse(expr, then_block, else_block) => {
                let ty = self.check_expr(expr)?;
                if ty == Type::Bool {
                    self.type_env.push(HashMap::new());
                    self.scope += 1;
                    for stmt in then_block {
                        self.check_stmt(stmt)?;
                    }
                    self.scope -= 1;
                    self.type_env.pop();
                    if let Some(stmts) = else_block {
                        self.scope += 1;
                        self.type_env.push(HashMap::new());
                        for stmt in stmts {
                            self.check_stmt(stmt)?;
                        }
                        self.type_env.pop();
                        self.scope -= 1;
                    }
                    Ok(())
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty]))
                }
            }
            Stmt::While(expr, stmts) => {
                let ty = self.check_expr(expr)?;
                if ty == Type::Bool {
                    self.scope += 1;
                    self.type_env.push(HashMap::new());
                    self.loop_depth += 1;
                    for stmt in stmts {
                        self.check_stmt(stmt)?;
                    }
                    self.loop_depth -= 1;
                    self.type_env.pop();
                    self.scope -= 1;
                    Ok(())
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty]))
                }
            }
            Stmt::For(var, in_ex, block) => {
                // in_ex should be a list
                let in_ex_ty = self.check_expr(in_ex)?;
                match in_ex_ty {
                    Type::List(ty) => {
                        let mut scope_env = HashMap::new();
                        scope_env.insert(var.clone(), *ty.clone());
                        self.scope += 1;
                        self.type_env.push(scope_env);
                        self.loop_depth += 1;
                        for stmt in block {
                            self.check_stmt(stmt)?;
                        }
                        self.loop_depth -= 1;
                        self.type_env.pop();
                        self.scope -= 1;
                        Ok(())
                    }
                    Type::Range => Ok(()),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![in_ex_ty],
                    )),
                }
            }
            Stmt::Decl(var, ex) => {
                let ty = self.check_expr(ex)?;
                match ty {
                    Type::Void => return Err(Error::TypeMismatch(vec![Type::Any], vec![ty])),
                    _ => {
                        if let Some(scope_env) = self.type_env.last_mut() {
                            scope_env.insert(var.clone(), ty.clone());
                        }
                    }
                }
                Ok(())
            }
            Stmt::Return(expr) => {
                let ty = self.check_expr(expr)?;
                if ty == self.func_ret_type {
                    Ok(())
                } else {
                    Err(Error::TypeMismatch(
                        vec![self.func_ret_type.clone()],
                        vec![ty],
                    ))
                }
            }
            Stmt::Break => {
                if self.loop_depth == 0 {
                    Err(Error::BreakOutsideLoop)
                } else {
                    Ok(())
                }
            }
            Stmt::Continue => {
                if self.loop_depth == 0 {
                    Err(Error::ContinueOutsideLoop)
                } else {
                    Ok(())
                }
            }
            Stmt::Comment => Ok(()),
            Stmt::Coroutine(expr) => {
                self.check_expr(expr)?;
                if let Expr::Call(_, _) = expr {
                    Ok(())
                } else {
                    Err(Error::CoroutineNotFunction)
                }
            }
            Stmt::ChanWrite(chan_name, expr) => {
                let chan_ty = self.find_var(chan_name)?;
                let expr_ty = self.check_expr(expr)?;
                match chan_ty {
                    Type::Chan(ty) => {
                        if *ty == expr_ty {
                            Ok(())
                        } else {
                            Err(Error::TypeMismatch(vec![*ty], vec![expr_ty]))
                        }
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![chan_ty],
                    )),
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum CFG {
    Empty,
    Seq(bool, Box<CFG>),
    Branch(bool, Box<CFG>, Box<CFG>),
}

fn gen_cfg(stmts: &[Stmt]) -> CFG {
    let res = stmts.split_first();
    let (stmt, stmts) = match res {
        Some((stmt, rest)) => (stmt, rest),
        None => return CFG::Empty,
    };
    match stmt {
        Stmt::Return(_) => CFG::Seq(true, Box::new(gen_cfg(stmts))),
        Stmt::IfElse(_, cons, alt) => {
            let cfg_alt = if alt.is_none() {
                gen_cfg(stmts)
            } else {
                let statements = vec![alt.as_ref().unwrap().as_slice(), stmts].concat();
                gen_cfg(statements.as_slice())
            };
            let statements = vec![cons.as_slice(), stmts].concat();
            CFG::Branch(
                false,
                Box::new(gen_cfg(statements.as_slice())),
                Box::new(cfg_alt),
            )
        }
        Stmt::While(_, block) | Stmt::For(_, _, block) => {
            let statements = vec![block.as_slice(), stmts].concat();
            CFG::Seq(false, Box::new(gen_cfg(statements.as_slice())))
        }
        _ => CFG::Seq(false, Box::new(gen_cfg(stmts))),
    }
}

fn validate_cfg(cfg: &CFG) -> bool {
    match cfg {
        CFG::Empty => false,
        CFG::Seq(b, cfg) => {
            if **cfg == CFG::Empty {
                *b
            } else {
                validate_cfg(cfg)
            }
        }
        CFG::Branch(_, cfg1, cfg2) => validate_cfg(cfg1) && validate_cfg(cfg2),
    }
}

pub fn check_program(program: &Program) -> Result<(), Error> {
    let mut func_env: FuncEnv = HashMap::new();
    for func in &program.funcs {
        let args_ty = func
            .args
            .iter()
            .map(|arg| arg.ty.clone())
            .collect::<Vec<_>>();
        let ret_ty = func.retty.clone();
        func_env.insert(func.name.clone(), (args_ty, ret_ty));
    }

    for func in &program.funcs {
        let mut checker = Checker::new(&func_env, func_env.get(&func.name).unwrap().1.clone());
        // add args to type env
        for arg in &func.args {
            checker
                .type_env
                .last_mut()
                .unwrap()
                .insert(arg.name.clone(), arg.ty.clone());
        }
        for stmt in &func.body {
            checker.check_stmt(stmt)?;
        }
        if func.retty != Type::Void {
            let cfg = gen_cfg(func.body.as_slice());
            println!("{:?}", func.name);
            println!("{:?}", cfg);
            if !validate_cfg(&cfg) {
                return Err(Error::NoReturn);
            }
        }
    }

    let mainfn = func_env.get("main");
    if mainfn.is_none() {
        return Err(Error::NoMain);
    }
    let mainfn = mainfn.unwrap();
    if mainfn.0.len() != 0 {
        return Err(Error::MainArgs);
    }
    if mainfn.1 != Type::Void {
        return Err(Error::MainNotVoid);
    }

    Ok(())
}

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
    TupleIndexOutOfBounds,
    TupleIndexNotInt,
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
            Error::TupleIndexOutOfBounds => {
                write!(f, "Tuple index out of bounds")
            }
            Error::TupleIndexNotInt => {
                write!(f, "Tuple index must be an integer")
            }
        }
    }
}

type TypeEnv = Vec<HashMap<String, Type>>;
pub type FuncEnv = HashMap<String, (Vec<Type>, Type)>;

fn builtin_fn(name: &str, args: &[Type]) -> Result<Type, Error> {
    match name {
        "print" => Ok(Type::Void),
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
                // match &args[0] {
                //     Type::List(_) => Ok(Type::Int),
                //     _ => Err(Error::TypeMismatch(
                //         vec![Type::List(Box::new(Type::Void))],
                //         vec![args[0].clone()],
                //     )),
                // }

                // arg can be either a list or a string
                Ok(Type::Int)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "clear" => Ok(Type::Void),
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
        }
        "randf" => {
            if args.len() == 0 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(0, args.len()))
            }
        }
        "exp" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "pow" => {
            if args.len() == 2 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(2, args.len()))
            }
        }
        "exit" => {
            if args.len() == 1 {
                Ok(Type::Void)
            } else {
                Err(Error::ArityMismatch(0, args.len()))
            }
        }
        "tanh" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "log" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "tcp_server" => {
            if args.len() == 2 {
                if let Type::Int = args[0] {
                    if let Type::Chan(_) = args[1].clone() {
                        // TODO: check that ch is [chan<string>]
                        Ok(Type::Void)
                    } else {
                        Err(Error::TypeMismatch(vec![Type::Int], vec![args[1].clone()]))
                    }
                } else {
                    Err(Error::TypeMismatch(vec![Type::Int], vec![args[0].clone()]))
                }
            } else {
                Err(Error::ArityMismatch(2, args.len()))
            }
        }
        "close_chan" => {
            if args.len() == 1 {
                if let Type::Chan(_) = args[0] {
                    Ok(Type::Void)
                } else {
                    Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![args[0].clone()],
                    ))
                }
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
        "is_open_chan" => {
            if args.len() == 1 {
                if let Type::Chan(_) = args[0] {
                    Ok(Type::Bool)
                } else {
                    Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![args[0].clone()],
                    ))
                }
            } else {
                Err(Error::ArityMismatch(1, args.len()))
            }
        }
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

    fn find_var(&self, name: &str) -> Result<TypedExpr, Error> {
        for i in (0..self.scope).rev() {
            if let Some(ty) = self.type_env[i].get(name) {
                return Ok(((ty).clone(), TyExpr::Variable(name.to_string())));
            }
        }
        Err(Error::UndefinedVariable(name.to_string()))
    }

    fn check_expr(&self, expr: &Expr) -> Result<TypedExpr, Error> {
        // println!("Checking expr: {:?}", expr);
        match expr {
            Expr::Literal(lit) => match lit {
                Lit::Int(_) => Ok((Type::Int, TyExpr::Literal(lit.clone()))),
                Lit::Bool(_) => Ok((Type::Bool, TyExpr::Literal(lit.clone()))),
                Lit::Char(_) => Ok((Type::Char, TyExpr::Literal(lit.clone()))),
                Lit::Str(_) => Ok((Type::Str, TyExpr::Literal(lit.clone()))),
                Lit::Double(_) => Ok((Type::Double, TyExpr::Literal(lit.clone()))),
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
                    if ty.0 == lhs_ty.0 {
                        Ok((Type::Void, TyExpr::Assign(Box::new(lhs_ty), Box::new(ty))))
                    } else {
                        Err(Error::TypeMismatch(vec![lhs_ty.0], vec![ty.0]))
                    }
                } else {
                    Err(Error::IncorrectLHS(lhs.clone()))
                }
            }
            Expr::Neg(e) => {
                let ty = self.check_expr(e)?;
                if ty.0 == Type::Int || ty.0 == Type::Double {
                    Ok((ty.0.clone(), TyExpr::Neg(Box::new(ty))))
                } else {
                    Err(Error::TypeMismatch(
                        vec![Type::Int, Type::Double],
                        vec![ty.0],
                    ))
                }
            }
            Expr::Not(e) => {
                let ty = self.check_expr(e)?;
                if ty.0 == Type::Bool {
                    Ok((Type::Bool, TyExpr::Not(Box::new(ty))))
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty.0]))
                }
            }
            Expr::List(list) => {
                let tyex = self.check_expr(&list.iter().next().unwrap())?;
                let mut ls = vec![tyex.clone()];
                for elem in list.iter().skip(1) {
                    let tyex2 = self.check_expr(elem)?;
                    if tyex2.0.clone() != tyex.0.clone() {
                        return Err(Error::TypeMismatch(vec![tyex.0], vec![tyex2.0]));
                    }
                    ls.push(tyex2);
                }
                Ok((Type::List(Box::new(tyex.0)), TyExpr::List(ls)))
            }
            Expr::Arith(op, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if *op == ArithOp::Add {
                    if (ty1.0 == Type::Str && ty2.0 == Type::Str)
                        || (ty1.0 == Type::Str && ty2.0 == Type::Char)
                        || (ty1.0 == Type::Char && ty2.0 == Type::Str)
                    {
                        Ok((
                            Type::Str,
                            TyExpr::Arith(op.clone(), Box::new(ty1), Box::new(ty2)),
                        ))
                    } else if ty1.0 == Type::Int && ty2.0 == Type::Int {
                        Ok((
                            Type::Int,
                            TyExpr::Arith(op.clone(), Box::new(ty1), Box::new(ty2)),
                        ))
                    } else if ty1.0 == Type::Double || ty2.0 == Type::Double {
                        Ok((
                            Type::Double,
                            TyExpr::Arith(op.clone(), Box::new(ty1), Box::new(ty2)),
                        ))
                    } else {
                        Err(Error::TypeMismatch(
                            vec![Type::Str, Type::Char],
                            vec![ty1.0, ty2.0],
                        ))
                    }
                } else if ty1.0 == Type::Int && ty2.0 == Type::Int {
                    Ok((
                        Type::Int,
                        TyExpr::Arith(op.clone(), Box::new(ty1), Box::new(ty2)),
                    ))
                } else if ty1.0 == Type::Double || ty2.0 == Type::Double {
                    Ok((
                        Type::Double,
                        TyExpr::Arith(op.clone(), Box::new(ty1), Box::new(ty2)),
                    ))
                } else {
                    Err(Error::TypeMismatch(
                        vec![Type::Int, Type::Double],
                        vec![ty1.0, ty2.0],
                    ))
                }
            }
            Expr::BoolOp(op, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1.0 == Type::Bool && ty2.0 == Type::Bool {
                    Ok((
                        Type::Bool,
                        TyExpr::BoolOp(op.clone(), Box::new(ty1), Box::new(ty2)),
                    ))
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty1.0, ty2.0]))
                }
            }
            Expr::CmpOp(op, ex1, ex2) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1.0 == ty2.0 {
                    Ok((
                        Type::Bool,
                        TyExpr::CmpOp(op.clone(), Box::new(ty1), Box::new(ty2)),
                    ))
                } else {
                    Err(Error::TypeMismatch(vec![ty1.0.clone()], vec![ty1.0, ty2.0]))
                }
            }
            Expr::Call(name, args) => {
                let argsty = args
                    .iter()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let res = builtin_fn(
                    name,
                    argsty
                        .iter()
                        .map(|(ty, _)| (ty).clone())
                        .collect::<Vec<_>>()
                        .as_slice(),
                );

                match res {
                    Ok(ty) => return Ok((ty, TyExpr::Call(name.clone(), argsty))),
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
                    if argty.0 != *formal_type {
                        return Err(Error::TypeMismatch(
                            vec![argty.0.clone()],
                            vec![formal_type.clone()],
                        ));
                    }
                }
                Ok((ret_type.clone(), TyExpr::Call(name.clone(), argsty)))
            }
            Expr::Subscr(expr, index) => {
                let ty = self.check_expr(expr)?;
                let index_ty = self.check_expr(index)?;
                match ty.0.clone() {
                    Type::List(t) => {
                        if index_ty.0 == Type::Int {
                            Ok((*t, TyExpr::Subscr(Box::new(ty.clone()), Box::new(index_ty))))
                        } else {
                            Err(Error::TypeMismatch(vec![Type::Int], vec![index_ty.0]))
                        }
                    }
                    Type::Map(key, val) => {
                        if *key.clone() == index_ty.0 {
                            Ok((*val, TyExpr::Subscr(Box::new(ty), Box::new(index_ty))))
                        } else {
                            Err(Error::TypeMismatch(vec![*key], vec![index_ty.0]))
                        }
                    }
                    Type::Str => {
                        if index_ty.0 == Type::Int {
                            Ok((Type::Char, TyExpr::Subscr(Box::new(ty), Box::new(index_ty))))
                        } else {
                            Err(Error::TypeMismatch(vec![Type::Int], vec![index_ty.0]))
                        }
                    }
                    Type::Tuple(tys) => {
                        // can only have integer literals as index
                        if let Expr::Literal(i) = *index.clone() {
                            if let Lit::Int(i) = i {
                                if i < tys.len() as i64 {
                                    Ok((
                                        tys[i as usize].clone(),
                                        TyExpr::Subscr(Box::new(ty), Box::new(index_ty)),
                                    ))
                                } else {
                                    Err(Error::TupleIndexOutOfBounds)
                                }
                            } else {
                                Err(Error::TupleIndexNotInt)
                            }
                        } else {
                            Err(Error::TypeMismatch(vec![Type::Int], vec![index_ty.0]))
                        }
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![ty.0],
                    )),
                }
            }
            Expr::ChanRead(chan_name) => {
                let ty = self.find_var(chan_name)?;
                match ty.0 {
                    Type::Chan(ty) => Ok((*ty.clone(), TyExpr::ChanRead(chan_name.clone()))),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![ty.0],
                    )),
                }
            }
            Expr::Make(ty, ex) => {
                let exty = if let Some(ex) = ex {
                    let exty = self.check_expr(ex)?;
                    if exty.0 == Type::Int {
                        Some(Box::new(exty))
                    } else {
                        return Err(Error::TypeMismatch(vec![(ty).clone()], vec![exty.0]));
                    }
                } else {
                    None
                };
                Ok(((ty).clone(), TyExpr::Make((ty).clone(), exty)))
            }
            Expr::Tuple(exprs) => {
                let tys = exprs
                    .iter()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((
                    Type::Tuple(tys.iter().map(|(ty, _)| ty.clone()).collect()),
                    TyExpr::Tuple(tys),
                ))
            }
            Expr::Range(ex1, ex2, cond) => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                match (&ty1, &ty2) {
                    (ty1, ty2) if ty1.0 == Type::Int && ty2.0 == Type::Int => Ok((
                        Type::Range,
                        TyExpr::Range(Box::new((ty1).clone()), Box::new((ty2).clone()), *cond),
                    )),
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Int, Type::Int],
                        vec![ty1.0, ty2.0],
                    )),
                }
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<TyStmt, Error> {
        // println!("check_stmt: {:?}", stmt);
        match stmt {
            Stmt::Expr(expr) => {
                let ex = self.check_expr(expr)?;
                Ok(TyStmt::Expr(Box::new(ex)))
            }
            Stmt::IfElse(expr, then_block, else_block) => {
                let cond = self.check_expr(expr)?;
                if cond.0.clone() == Type::Bool {
                    self.type_env.push(HashMap::new());
                    self.scope += 1;
                    let mut then = Vec::new();
                    for stmt in then_block {
                        let res = self.check_stmt(stmt)?;
                        then.push(res);
                    }
                    self.scope -= 1;
                    self.type_env.pop();
                    let else_ = if let Some(stmts) = else_block {
                        self.scope += 1;
                        self.type_env.push(HashMap::new());
                        let mut else_ = Vec::new();
                        for stmt in stmts {
                            let res = self.check_stmt(stmt)?;
                            else_.push(res);
                        }
                        self.type_env.pop();
                        self.scope -= 1;
                        Some(else_)
                    } else {
                        None
                    };
                    Ok(TyStmt::IfElse(Box::new(cond), then, else_))
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![cond.0]))
                }
            }
            Stmt::While(expr, stmts) => {
                let ty = self.check_expr(expr)?;
                if ty.0 == Type::Bool {
                    self.scope += 1;
                    self.type_env.push(HashMap::new());
                    self.loop_depth += 1;
                    let mut checked_stmts = Vec::new();
                    for stmt in stmts {
                        checked_stmts.push(self.check_stmt(stmt)?);
                    }
                    self.loop_depth -= 1;
                    self.type_env.pop();
                    self.scope -= 1;
                    Ok(TyStmt::While(Box::new(ty), checked_stmts))
                } else {
                    Err(Error::TypeMismatch(vec![Type::Bool], vec![ty.0]))
                }
            }
            Stmt::For(var, in_ex, block) => {
                // in_ex should be a list
                let in_ex_ty = self.check_expr(in_ex)?;
                match in_ex_ty.0.clone() {
                    Type::List(ty) => {
                        let mut scope_env = HashMap::new();
                        scope_env.insert(var.clone(), *ty.clone());
                        self.scope += 1;
                        self.type_env.push(scope_env);
                        self.loop_depth += 1;
                        let mut checked_stmts = Vec::new();
                        for stmt in block {
                            checked_stmts.push(self.check_stmt(stmt)?);
                        }
                        self.loop_depth -= 1;
                        self.type_env.pop();
                        self.scope -= 1;
                        Ok(TyStmt::For(var.clone(), Box::new(in_ex_ty), checked_stmts))
                    }
                    Type::Range => {
                        let mut scope_env = HashMap::new();
                        scope_env.insert(var.clone(), Type::Int);
                        self.scope += 1;
                        self.type_env.push(scope_env);
                        self.loop_depth += 1;
                        let mut checked_stmts = Vec::new();
                        for stmt in block {
                            checked_stmts.push(self.check_stmt(stmt)?);
                        }
                        self.loop_depth -= 1;
                        self.type_env.pop();
                        self.scope -= 1;
                        Ok(TyStmt::For(var.clone(), Box::new(in_ex_ty), checked_stmts))
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::List(Box::new(Type::Void))],
                        vec![in_ex_ty.0],
                    )),
                }
            }
            Stmt::Decl(var, ex) => {
                let ty = self.check_expr(ex)?;
                match ty.0 {
                    Type::Void => return Err(Error::TypeMismatch(vec![Type::Any], vec![ty.0])),
                    _ => {
                        if let Some(scope_env) = self.type_env.last_mut() {
                            scope_env.insert(var.clone(), ty.0.clone());
                        }
                    }
                }
                Ok(TyStmt::Decl(var.clone(), Box::new(ty.clone())))
            }
            Stmt::Return(expr) => {
                let ty = self.check_expr(expr)?;
                if ty.0 == self.func_ret_type {
                    Ok(TyStmt::Return(Box::new(ty)))
                } else {
                    Err(Error::TypeMismatch(
                        vec![self.func_ret_type.clone()],
                        vec![ty.0],
                    ))
                }
            }
            Stmt::Break => {
                if self.loop_depth == 0 {
                    Err(Error::BreakOutsideLoop)
                } else {
                    Ok(TyStmt::Break)
                }
            }
            Stmt::Continue => {
                if self.loop_depth == 0 {
                    Err(Error::ContinueOutsideLoop)
                } else {
                    Ok(TyStmt::Continue)
                }
            }
            Stmt::Comment => Ok(TyStmt::Comment),
            Stmt::Coroutine(expr) => {
                let checked = self.check_expr(expr)?;
                if let Expr::Call(_, _) = expr {
                    Ok(TyStmt::Coroutine(checked))
                } else {
                    Err(Error::CoroutineNotFunction)
                }
            }
            Stmt::ChanWrite(chan_name, expr) => {
                let chan_ty = self.find_var(chan_name)?;
                let expr_ty = self.check_expr(expr)?;
                match chan_ty.0 {
                    Type::Chan(ty) => {
                        if *ty.clone() == expr_ty.0 {
                            Ok(TyStmt::ChanWrite(chan_name.clone(), Box::new(expr_ty)))
                        } else {
                            Err(Error::TypeMismatch(vec![*ty.clone()], vec![expr_ty.0]))
                        }
                    }
                    _ => Err(Error::TypeMismatch(
                        vec![Type::Chan(Box::new(Type::Void))],
                        vec![chan_ty.0],
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

pub fn check_program(program: &Program) -> Result<(FuncEnv, TyProgram), Error> {
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

    let mut typed_funcs = Vec::new();

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
        let mut checked_stmts = Vec::new();
        for stmt in &func.body {
            checked_stmts.push(checker.check_stmt(stmt)?);
        }
        typed_funcs.push(TyFunc {
            name: func.name.clone(),
            args: func.args.clone(),
            retty: func.retty.clone(),
            body: checked_stmts,
        });
        if func.retty != Type::Void {
            let cfg = gen_cfg(func.body.as_slice());
            // println!("{:?}", func.name);
            // println!("{:?}", cfg);
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

    let typed_program = TyProgram { funcs: typed_funcs };
    Ok((func_env, typed_program))
}

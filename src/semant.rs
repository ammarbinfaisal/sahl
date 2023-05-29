use crate::syntax::*;
use std::{collections::HashMap, vec};

pub enum Error {
    TypeMismatch(Vec<Type>, Vec<Type>),
    ArityMismatch(usize, usize),
    UndefinedVariable(String),
    UndefinedFunction(String),
    CoroutineNotFunction,
    ContinueOutsideLoop,
    BreakOutsideLoop,
    IncorrectLHS(Expr),
    NoReturn(String),
    NoMain,
    MainArgs,
    MainNotVoid,
    TupleIndexOutOfBounds(usize, usize),
    CastError(Type, Type),
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
            Error::NoReturn(name) => {
                write!(f, "Function {} does not return", name)
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
            Error::TupleIndexOutOfBounds(index, len) => {
                write!(f, "Tuple index {} out of bounds for tuple of length {}", index, len)
            }
            Error::CastError(from, to) => {
                write!(f, "Cannot cast from {:?} to {:?}", from, to)
            }
        }
    }
}

type TypeEnv = Vec<HashMap<String, Type>>;
pub type FuncEnv = HashMap<String, (Vec<Type>, Type)>;

fn builtin_fn(name: &str, args: &[Type], start: usize, end: usize) -> Result<Type, Spanned<Error>> {
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
                            Err((
                                start,
                                Error::TypeMismatch(vec![ty.clone()], vec![args[1].clone()]),
                                end,
                            ))
                        }
                    }
                    _ => Err((
                        start,
                        Error::TypeMismatch(
                            vec![Type::List(Box::new(Type::Void))],
                            vec![args[0].clone()],
                        ),
                        end,
                    )),
                }
            } else {
                Err((start, Error::ArityMismatch(2, args.len()), end))
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
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "clear" => Ok(Type::Void),
        "sleep" => {
            if args.len() == 1 {
                Ok(Type::Void)
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "rand" => {
            if args.len() == 2 {
                Ok(Type::Int)
            } else {
                Err((start, Error::ArityMismatch(2, args.len()), end))
            }
        }
        "randf" => {
            if args.len() == 0 {
                Ok(Type::Double)
            } else {
                Err((start, Error::ArityMismatch(0, args.len()), end))
            }
        }
        "exp" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "pow" => {
            if args.len() == 2 {
                Ok(Type::Double)
            } else {
                Err((start, Error::ArityMismatch(2, args.len()), end))
            }
        }
        "exit" => {
            if args.len() == 1 {
                Ok(Type::Void)
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "tanh" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "log" => {
            if args.len() == 1 {
                Ok(Type::Double)
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "tcp_server" => {
            if args.len() == 2 {
                if let Type::Int = args[0] {
                    if let Type::Chan(_) = args[1].clone() {
                        // TODO: check that ch is [chan<string>]
                        Ok(Type::Void)
                    } else {
                        Err((
                            start,
                            Error::TypeMismatch(
                                vec![Type::Chan(Box::new(Type::Void))],
                                vec![args[1].clone()],
                            ),
                            end,
                        ))
                    }
                } else {
                    Err((
                        start,
                        Error::TypeMismatch(vec![Type::Int], vec![args[0].clone()]),
                        end,
                    ))
                }
            } else {
                Err((start, Error::ArityMismatch(2, args.len()), end))
            }
        }
        "close_chan" => {
            if args.len() == 1 {
                if let Type::Chan(_) = args[0] {
                    Ok(Type::Void)
                } else {
                    Err((
                        start,
                        Error::TypeMismatch(
                            vec![Type::Chan(Box::new(Type::Void))],
                            vec![args[0].clone()],
                        ),
                        end,
                    ))
                }
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        "is_open_chan" => {
            if args.len() == 1 {
                if let Type::Chan(_) = args[0] {
                    Ok(Type::Bool)
                } else {
                    Err((
                        start,
                        Error::TypeMismatch(
                            vec![Type::Chan(Box::new(Type::Void))],
                            vec![args[0].clone()],
                        ),
                        end,
                    ))
                }
            } else {
                Err((start, Error::ArityMismatch(1, args.len()), end))
            }
        }
        _ => Err((start, Error::UndefinedFunction(name.to_string()), end)),
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

    fn find_var(&self, name: &str, start: usize, end: usize) -> Result<Type, Spanned<Error>> {
        for i in (0..self.scope).rev() {
            if let Some(ty) = self.type_env[i].get(name) {
                return Ok(ty.clone());
            }
        }
        Err((start, Error::UndefinedVariable(name.to_string()), end))
    }

    fn check_expr(&self, expr: &mut Spanned<Expr>) -> Result<Type, Spanned<Error>> {
        // println!("Checking expr: {:?}", expr);
        let immut_expr = expr.1.clone();
        match &mut expr.1 {
            Expr::Literal { lit: _, ty } => Ok(ty.clone()),
            Expr::Variable { name, ty } => {
                if let Some(t) = ty {
                    Ok(t.clone())
                } else {
                    let t = self.find_var(name, expr.0, expr.2)?;
                    *ty = Some(t.clone());
                    Ok(t)
                }
            }
            Expr::Assign { left, right } => {
                let ty = self.check_expr(right)?;
                let correct_lhs = match left.1.clone() {
                    Expr::Variable { .. } => true,
                    Expr::Subscr { .. } => true,
                    _ => false,
                };
                if correct_lhs {
                    let lhs_ty = self.check_expr(left)?;
                    if ty == lhs_ty {
                        Ok(Type::Void)
                    } else {
                        Err((left.0, Error::TypeMismatch(vec![ty], vec![lhs_ty]), left.2))
                    }
                } else {
                    Err((left.0, Error::IncorrectLHS(immut_expr), left.2.clone()))
                }
            }
            Expr::Neg { expr, ty: t } => {
                let ty = self.check_expr(expr)?;
                if ty == Type::Int || ty == Type::Double {
                    *t = Some(ty.clone());
                    Ok(ty)
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Int, Type::Double], vec![ty]),
                        expr.2,
                    ))
                }
            }
            Expr::Not { expr, ty: t } => {
                let ty = self.check_expr(expr)?;
                if ty == Type::Bool {
                    *t = Some(ty.clone());
                    Ok(Type::Bool)
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Bool], vec![ty]),
                        expr.2,
                    ))
                }
            }
            Expr::List { exprs, ty: t } => {
                let tyex = self.check_expr(exprs.iter_mut().next().unwrap())?;
                for elem in exprs.iter_mut().skip(1) {
                    let tyex2 = self.check_expr(elem)?;
                    if tyex2.clone() != tyex.clone() {
                        return Err((elem.0, Error::TypeMismatch(vec![tyex], vec![tyex2]), elem.2));
                    }
                }
                *t = Some(Type::List(Box::new(tyex.clone())));
                Ok(Type::List(Box::new(tyex)))
            }
            Expr::Arith {
                op,
                left: ex1,
                right: ex2,
                ty: t,
            } => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if *op == ArithOp::Add {
                    if (ty1 == Type::Str && ty2 == Type::Str)
                        || (ty1 == Type::Str && ty2 == Type::Char)
                        || (ty1 == Type::Char && ty2 == Type::Str)
                    {
                        *t = Some(Type::Str);
                        Ok(Type::Str)
                    } else if ty1 == Type::Int && ty2 == Type::Int {
                        *t = Some(Type::Int);
                        Ok(Type::Int)
                    } else if ty1 == Type::Double || ty2 == Type::Double {
                        *t = Some(Type::Double);
                        Ok(Type::Double)
                    } else {
                        Err((
                            expr.0,
                            Error::TypeMismatch(vec![Type::Str, Type::Char], vec![ty1, ty2]),
                            expr.2,
                        ))
                    }
                } else if ty1 == Type::Int && ty2 == Type::Int {
                    *t = Some(Type::Int);
                    Ok(Type::Int)
                } else if ty1 == Type::Double && ty2 == Type::Double {
                    *t = Some(Type::Double);
                    Ok(Type::Double)
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![ty1], vec![ty2]),
                        expr.2,
                    ))
                }
            }
            Expr::BoolOp {
                op: _,
                left: ex1,
                right: ex2,
                ty: t,
            } => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1 == Type::Bool && ty2 == Type::Bool {
                    *t = Some(Type::Bool);
                    Ok(Type::Bool)
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Bool], vec![ty1, ty2]),
                        expr.2,
                    ))
                }
            }
            Expr::BitOp { op: _, left, right, ty } => {
                let ty1 = self.check_expr(left)?;
                let ty2 = self.check_expr(right)?;
                if ty1 == Type::Int && ty2 == Type::Int {
                    *ty = Some(Type::Int);
                    Ok(Type::Int)
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Int], vec![ty1, ty2]),
                        expr.2,
                    ))
                }
            }
            Expr::CmpOp {
                op: _,
                left: ex1,
                right: ex2,
                ty: t,
            } => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1 == ty2 {
                    *t = Some(Type::Bool);
                    Ok(Type::Bool)
                } else {
                    Err((expr.0, Error::TypeMismatch(vec![ty1], vec![ty2]), expr.2))
                }
            }
            Expr::Call { name, args, ty: t } => {
                let argsty = args
                    .iter_mut()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let res = builtin_fn(
                    name,
                    argsty
                        .iter()
                        .map(|ty| ty.clone())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    expr.0,
                    expr.2,
                );

                match res {
                    Err((_, Error::UndefinedFunction(_), _)) => {} 
                    Err(e) => return Err(e),
                    Ok(ty) => {
                        *t = Some(ty.clone());
                        return Ok(ty);
                    }
                }

                let (formal_types, ret_type) = self.func_env.get(name).ok_or((
                    expr.0,
                    Error::UndefinedFunction(name.clone()),
                    expr.2,
                ))?;

                if argsty.len() != formal_types.len() {
                    return Err((
                        expr.0,
                        Error::ArityMismatch(formal_types.len(), argsty.len()),
                        expr.2,
                    ));
                }

                for (argty, formal_type) in argsty.iter().zip(formal_types) {
                    if argty != formal_type {
                        return Err((
                            expr.0,
                            Error::TypeMismatch(vec![argty.clone()], vec![formal_type.clone()]),
                            expr.2,
                        ));
                    }
                }
                *t = Some(ret_type.clone());
                Ok(ret_type.clone())
            }
            Expr::Cast { expr, ty } => {
                let tyex = self.check_expr(expr)?;
                if tyex == Type::Int && *ty == Type::Double {
                    Ok(Type::Double)
                } else if tyex == Type::Char && *ty == Type::Int {
                    Ok(Type::Int)
                } else if tyex == Type::Int && *ty == Type::Char {
                    Ok(Type::Char)
                } else {
                    Err((
                        expr.0,
                        Error::CastError(tyex.clone(), ty.clone()),
                        expr.2,
                    ))
                }
            }
            Expr::Subscr { expr, index, ty: t } => {
                let ty = self.check_expr(expr)?;
                let index_ty = self.check_expr(index)?;
                match ty.clone() {
                    Type::List(tyy) => {
                        if index_ty == Type::Int {
                            *t = Some(*tyy.clone());
                            Ok(*tyy)
                        } else {
                            Err((
                                expr.0,
                                Error::TypeMismatch(vec![Type::Int], vec![index_ty]),
                                expr.2,
                            ))
                        }
                    }
                    Type::Map(key, val) => {
                        if *key.clone() == index_ty {
                            *t = Some(*val.clone());
                            Ok(*val)
                        } else {
                            Err((
                                expr.0,
                                Error::TypeMismatch(vec![*key.clone()], vec![index_ty]),
                                expr.2,
                            ))
                        }
                    }
                    Type::Str => {
                        if index_ty == Type::Int {
                            *t = Some(Type::Char);
                            Ok(Type::Char)
                        } else {
                            Err((
                                expr.0,
                                Error::TypeMismatch(vec![Type::Int], vec![index_ty]),
                                expr.2,
                            ))
                        }
                    }
                    Type::Tuple(tys) => {
                        // can only have integer literals as index
                        if let Expr::Literal { lit, ty: _ } = index.1.clone() {
                            if let Lit::Int(i) = lit {
                                if i < tys.len() as i64 {
                                    Ok(tys[i as usize].clone())
                                } else {
                                    Err((
                                        expr.0,
                                        Error::TupleIndexOutOfBounds(i as usize, tys.len()),
                                        expr.2,
                                    ))
                                }
                            } else {
                                Err((
                                    expr.0,
                                    Error::TypeMismatch(vec![Type::Int], vec![index_ty]),
                                    expr.2,
                                ))
                            }
                        } else {
                            Err((
                                expr.0,
                                Error::TypeMismatch(vec![Type::Int], vec![index_ty]),
                                expr.2,
                            ))
                        }
                    }
                    _ => Err((
                        expr.0,
                        Error::TypeMismatch(
                            vec![Type::List(Box::new(Type::Void))],
                            vec![ty.clone()],
                        ),
                        expr.2,
                    )),
                }
            }
            Expr::ChanRead { name, ty: t } => {
                let ty = self.find_var(&name, expr.0, expr.2)?;
                match ty {
                    Type::Chan(ty) => {
                        *t = Some(*ty.clone());
                        Ok(*ty)
                    }
                    _ => Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Chan(Box::new(Type::Void))], vec![ty]),
                        expr.2,
                    )),
                }
            }
            Expr::Make { ty, expr: ex } => {
                if let Some(ex) = ex {
                    let exty = self.check_expr(ex)?;
                    if exty == Type::Int {
                        Ok(ty.clone())
                    } else {
                        Err((
                            expr.0,
                            Error::TypeMismatch(vec![Type::Int], vec![exty]),
                            expr.2,
                        ))
                    }
                } else {
                    Ok(ty.clone())
                }
            }
            Expr::Tuple { exprs, ty: t } => {
                let tys = exprs
                    .iter_mut()
                    .map(|e| self.check_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let tyy = Type::Tuple(tys.iter().map(|ty| ty.clone()).collect::<Vec<_>>());
                *t = Some(tyy.clone());
                Ok(tyy)
            }
            Expr::Range {
                start: ex1,
                end: ex2,
                inclusive: _,
            } => {
                let ty1 = self.check_expr(ex1)?;
                let ty2 = self.check_expr(ex2)?;
                if ty1 == Type::Int && ty2 == Type::Int {
                    Ok(Type::List(Box::new(Type::Int)))
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Int], vec![ty1, ty2]),
                        expr.2,
                    ))
                }
            }
        }
    }

    fn check_stmt(&mut self, stmt: &mut Spanned<Stmt>) -> Result<(), Spanned<Error>> {
        // println!("check_stmt: {:?}", stmt);
        match &mut stmt.1 {
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
                Ok(())
            }
            Stmt::IfElse(expr, then_block, else_block) => {
                let cond = self.check_expr(expr)?;
                if cond.clone() == Type::Bool {
                    self.type_env.push(HashMap::new());
                    self.scope += 1;
                    let mut then = Vec::new();
                    for stmt in then_block {
                        let res = self.check_stmt(stmt)?;
                        then.push(res);
                    }
                    self.scope -= 1;
                    self.type_env.pop();
                    if let Some(stmts) = else_block {
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
                    Ok(())
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Bool], vec![cond]),
                        expr.2,
                    ))
                }
            }
            Stmt::While(expr, stmts) => {
                let ty = self.check_expr(expr)?;
                if ty == Type::Bool {
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
                    Ok(())
                } else {
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![Type::Bool], vec![ty]),
                        expr.2,
                    ))
                }
            }
            Stmt::For(var, in_ex, block) => {
                // in_ex should be a list
                let in_ex_ty = self.check_expr(in_ex)?;
                match in_ex_ty.clone() {
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
                        Ok(())
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
                        Ok(())
                    }
                    _ => Err((
                        in_ex.0,
                        Error::TypeMismatch(vec![Type::List(Box::new(Type::Any))], vec![in_ex_ty]),
                        in_ex.2,
                    )),
                }
            }
            Stmt::Decl(var, ex) => {
                let ty = self.check_expr(ex)?;
                match ty {
                    Type::Void => {
                        return Err((
                            ex.0,
                            Error::TypeMismatch(vec![Type::Void], vec![Type::Any]),
                            ex.2,
                        ))
                    }
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
                    Err((
                        expr.0,
                        Error::TypeMismatch(vec![self.func_ret_type.clone()], vec![ty]),
                        expr.2,
                    ))
                }
            }
            Stmt::Break => {
                if self.loop_depth == 0 {
                    Err((stmt.0, Error::BreakOutsideLoop, stmt.2))
                } else {
                    Ok(())
                }
            }
            Stmt::Continue => {
                if self.loop_depth == 0 {
                    Err((stmt.0, Error::ContinueOutsideLoop, stmt.2))
                } else {
                    Ok(())
                }
            }
            Stmt::Coroutine(expr) => {
                self.check_expr(expr)?;
                if let Expr::Call { .. } = expr.1 {
                    Ok(())
                } else {
                    Err((expr.0, Error::CoroutineNotFunction, expr.2))
                }
            }
            Stmt::ChanWrite(chan_name, expr) => {
                let chan_ty = self.find_var(chan_name, stmt.0, stmt.2)?;
                let expr_ty = self.check_expr(expr)?;
                match chan_ty {
                    Type::Chan(ty) => {
                        if *ty.clone() == expr_ty {
                            Ok(())
                        } else {
                            Err((
                                stmt.0,
                                Error::TypeMismatch(vec![*ty.clone()], vec![expr_ty]),
                                stmt.2,
                            ))
                        }
                    }
                    _ => Err((
                        stmt.0,
                        Error::TypeMismatch(vec![Type::Chan(Box::new(Type::Void))], vec![chan_ty]),
                        stmt.2,
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
                // convert Vec<(usize, Stmt, usize)> to Vec<Stmt>
                let statements = alt
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|(_, stmt, _)| stmt.clone())
                    .collect::<Vec<Stmt>>();
                let sts = vec![statements.as_slice(), stmts].concat();
                gen_cfg(sts.as_slice())
            };
            let statements = cons
                .iter()
                .map(|(_, stmt, _)| stmt.clone())
                .collect::<Vec<Stmt>>();
            let sts = vec![statements.as_slice(), stmts].concat();
            CFG::Branch(
                false,
                Box::new(gen_cfg(sts.as_slice())),
                Box::new(cfg_alt),
            )
        }
        Stmt::While(_, block) | Stmt::For(_, _, block) => {
            let statements = block
                .iter()
                .map(|(_, stmt, _)| stmt.clone())
                .collect::<Vec<Stmt>>()
                .clone();
            let sts = vec![statements.as_slice(), stmts].concat();
            CFG::Seq(false, Box::new(gen_cfg(sts.as_slice())))
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

pub fn check_program(program: &mut Program) -> Result<FuncEnv, Spanned<Error>> {
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

    for func in program.funcs.iter_mut() {
        let mut checker = Checker::new(&func_env, func_env.get(&func.name).unwrap().1.clone());
        // add args to type env
        for arg in &func.args {
            checker
                .type_env
                .last_mut()
                .unwrap()
                .insert(arg.name.clone(), arg.ty.clone());
        }
        for stmt in func.body.iter_mut() {
            checker.check_stmt(stmt)?;
        }
        if func.retty != Type::Void {
            let cfg = gen_cfg(
                func.body
                    .iter()
                    .map(|(_, stmt, _)| stmt.clone())
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
            // println!("{:?}", func.name);
            // println!("{:?}", cfg);
            if !validate_cfg(&cfg) {
                return Err((
                    func.body[func.body.len() - 1].0,
                    Error::NoReturn(func.name.clone()),
                    func.body[func.body.len() - 1].2,
                ));
            }
        }
    }

    let mainfn = func_env.get("main");
    if mainfn.is_none() {
        return Err((0, Error::NoMain, 0));
    }
    let mainfn = mainfn.unwrap();
    if mainfn.0.len() != 0 {
        return Err((0, Error::MainArgs, 0));
    }
    if mainfn.1 != Type::Void {
        return Err((0, Error::MainNotVoid, 0));
    }
    Ok(func_env)
}

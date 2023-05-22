#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Char,
    Bool,
    Void,
    Double,
    Any,
    List(Box<Type>),
    Chan(Box<Type>),
    Tuple(Vec<Type>),
    Map(Box<Type>, Box<Type>),
    Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Char(u8),
    Bool(bool),
    Str(Vec<u8>),
    List(Vec<Expr>),
    Double(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Variable(String),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    Range(Box<Expr>, Box<Expr>, bool),
    Arith(ArithOp, Box<Expr>, Box<Expr>),
    BoolOp(BoolOp, Box<Expr>, Box<Expr>),
    CmpOp(CmpOp, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Subscr(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Make(Type, Option<Box<Expr>>),
    Tuple(Vec<Expr>),
    ChanRead(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyExpr {
    Literal(Lit),
    Variable(String),
    Neg(Box<TypedExpr>),
    Not(Box<TypedExpr>),
    Range(Box<TypedExpr>, Box<TypedExpr>, bool),
    Arith(ArithOp, Box<TypedExpr>, Box<TypedExpr>),
    BoolOp(BoolOp, Box<TypedExpr>, Box<TypedExpr>),
    CmpOp(CmpOp, Box<TypedExpr>, Box<TypedExpr>),
    Call(String, Vec<TypedExpr>),
    Subscr(Box<TypedExpr>, Box<TypedExpr>),
    Assign(Box<TypedExpr>, Box<TypedExpr>),
    Make(Type, Option<Box<TypedExpr>>),
    Tuple(Vec<TypedExpr>),
    ChanRead(String),
}

pub type TypedExpr = (Type, TyExpr);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    Decl(String, Box<Expr>),
    For(String, Box<Expr>, Vec<Stmt>),
    While(Box<Expr>, Vec<Stmt>),
    IfElse(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>),
    Return(Box<Expr>),
    Coroutine(Expr),
    ChanWrite(String, Box<Expr>),
    Continue,
    Comment,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyStmt {
    Expr(Box<TypedExpr>),
    Decl(String, Box<TypedExpr>),
    For(String, Box<TypedExpr>, Vec<TyStmt>),
    While(Box<TypedExpr>, Vec<TyStmt>),
    IfElse(Box<TypedExpr>, Vec<TyStmt>, Option<Vec<TyStmt>>),
    Return(Box<TypedExpr>),
    Coroutine(TypedExpr),
    ChanWrite(String, Box<TypedExpr>),
    Continue,
    Comment,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<Param>,
    pub body: Vec<Stmt>,
    pub retty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyFunc {
    pub name: String,
    pub args: Vec<Param>,
    pub body: Vec<TyStmt>,
    pub retty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub funcs: Vec<Func>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyProgram {
    pub funcs: Vec<TyFunc>,
}

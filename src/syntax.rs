#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Char,
    Bool,
    Void,
    Any,
    List(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Char(u8),
    Bool(bool),
    Str(Vec<u8>),
    List(Vec<Expr>),
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
    Arith(ArithOp, Box<Expr>, Box<Expr>),
    BoolOp(BoolOp, Box<Expr>, Box<Expr>),
    CmpOp(CmpOp, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Subscr(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Make(Type, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    Decl(String, Box<Expr>),
    For(String, Box<Expr>, Vec<Stmt>),
    While(Box<Expr>, Vec<Stmt>),
    IfElse(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>),
    Return(Box<Expr>),
    Coroutine(Expr),
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
pub struct Program {
    pub funcs: Vec<Func>,
    pub main: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Char,
    Bool,
    Void,
    Float,
    List(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Char(u8),
    Bool(bool),
    Float(f64),
    Str(Vec<u8>),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Variable(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
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

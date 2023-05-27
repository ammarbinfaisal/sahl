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
    Literal {
        lit: Lit,
        ty: Type,
    },
    Variable {
        name: String,
        ty: Option<Type>,
    },
    Neg {
        expr: Box<Expr>,
        ty: Option<Type>,
    },
    Not {
        expr: Box<Expr>,
        ty: Option<Type>,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    Arith {
        op: ArithOp,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Option<Type>,
    },
    BoolOp {
        op: BoolOp,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Option<Type>,
    },
    CmpOp {
        op: CmpOp,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Option<Type>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    Subscr {
        expr: Box<Expr>,
        index: Box<Expr>,
        ty: Option<Type>,
    },
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Make {
        ty: Type,
        expr: Option<Box<Expr>>,
    },
    Tuple {
        exprs: Vec<Expr>,
        ty: Option<Type>,
    },
    ChanRead {
        name: String,
        ty: Option<Type>,
    },
    List {
        exprs: Vec<Expr>,
        ty: Option<Type>,
    },
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Literal { ty, .. } => ty.clone(),
            Expr::Variable { ty, .. } => ty.clone().unwrap(),
            Expr::Neg { ty, .. } => ty.clone().unwrap(),
            Expr::Not { ty, .. } => ty.clone().unwrap(),
            Expr::Range { .. } => Type::Range,
            Expr::Arith { ty, .. } => ty.clone().unwrap(),
            Expr::BoolOp { ty, .. } => ty.clone().unwrap(),
            Expr::CmpOp { ty, .. } => ty.clone().unwrap(),
            Expr::Call { ty, .. } => ty.clone().unwrap(),
            Expr::Subscr { ty, .. } => ty.clone().unwrap(),
            Expr::Assign { .. } => Type::Void,
            Expr::Make { ty, .. } => ty.clone(),
            Expr::Tuple { ty, .. } => ty.clone().unwrap(),
            Expr::ChanRead { ty, .. } => ty.clone().unwrap(),
            Expr::List { ty, .. } => ty.clone().unwrap(),
        }
    }
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
    ChanWrite(String, Box<Expr>),
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
}

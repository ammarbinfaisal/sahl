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

impl Type {
    #[inline]
    pub fn is_heap_type(&self) -> bool {
        match self {
            Type::List(_) | Type::Map(_, _) | Type::Chan(_) | Type::Str => true,
            _ => false,
        }
    }
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
pub enum BitOp {
    And,
    Or,
    Xor,
    Shl,
    Shr,
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
        expr: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Not {
        expr: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Range {
        start: Box<Spanned<Expr>>,
        end: Box<Spanned<Expr>>,
        inclusive: bool,
    },
    Arith {
        op: ArithOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    BoolOp {
        op: BoolOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    CmpOp {
        op: CmpOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    BitOp {
        op: BitOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Call {
        name: Box<Expr>,
        args: Vec<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Subscr {
        expr: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Assign {
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Make {
        ty: Type,
        expr: Option<Box<Spanned<Expr>>>,
    },
    Tuple {
        exprs: Vec<Spanned<Expr>>,
        ty: Option<Type>,
    },
    ChanRead {
        name: String,
        ty: Option<Type>,
    },
    List {
        exprs: Vec<Spanned<Expr>>,
        ty: Option<Type>,
    },
    Cast {
        expr: Box<Spanned<Expr>>,
        ty: Type,
    },
    Ref {
        expr: Box<Expr>,
        ty: Option<Type>,
        usage: bool,
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
            Expr::BitOp { ty, .. } => ty.clone().unwrap(),
            Expr::Call { ty, .. } => ty.clone().unwrap(),
            Expr::Subscr { ty, .. } => ty.clone().unwrap(),
            Expr::Assign { .. } => Type::Void,
            Expr::Make { ty, .. } => ty.clone(),
            Expr::Tuple { ty, .. } => ty.clone().unwrap(),
            Expr::ChanRead { ty, .. } => ty.clone().unwrap(),
            Expr::List { ty, .. } => ty.clone().unwrap(),
            Expr::Cast { ty, .. } => ty.clone(),
            Expr::Ref { ty, .. } => ty.clone().unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Spanned<Expr>>),
    Decl(String, Box<Spanned<Expr>>),
    For(String, Box<Spanned<Expr>>, Vec<Spanned<Stmt>>),
    While(Box<Spanned<Expr>>, Vec<Spanned<Stmt>>),
    IfElse(
        Box<Spanned<Expr>>,
        Vec<Spanned<Stmt>>,
        Option<Vec<Spanned<Stmt>>>,
    ),
    Return(Box<Spanned<Expr>>),
    Coroutine(Spanned<Expr>),
    ChanWrite(String, Box<Spanned<Expr>>),
    Block(Vec<Spanned<Stmt>>),
    Continue,
    Break,
}

pub type Spanned<T> = (usize, T, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<Param>,
    pub body: Vec<Spanned<Stmt>>,
    pub retty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub funcs: Vec<Func>,
}

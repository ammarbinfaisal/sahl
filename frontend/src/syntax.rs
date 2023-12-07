use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'src> {
    Int,
    Str,
    Char,
    Bool,
    Void,
    Double,
    Any,
    List(Box<Type<'src>>),
    Chan(Box<Type<'src>>),
    Tuple(Vec<Type<'src>>),
    Map(Box<Type<'src>>, Box<Type<'src>>),
    Range,
    Ref(Box<Type<'src>>),
    Custom(&'src str),
    Variant(Vec<(&'src str, Type<'src>)>),
}

impl<'src> Type<'src> {
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
    Str(Arc<String>),
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
pub enum Expr<'src> {
    Literal {
        lit: Lit,
        ty: Type<'src>,
    },
    Variable {
        name: &'src str,
        ty: Option<Type<'src>>,
    },
    Neg {
        expr: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Not {
        expr: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    BitNot {
        expr: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Range {
        start: Box<Spanned<Expr<'src>>>,
        end: Box<Spanned<Expr<'src>>>,
        inclusive: bool,
    },
    Arith {
        op: ArithOp,
        left: Box<Spanned<Expr<'src>>>,
        right: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    BoolOp {
        op: BoolOp,
        left: Box<Spanned<Expr<'src>>>,
        right: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    CmpOp {
        op: CmpOp,
        left: Box<Spanned<Expr<'src>>>,
        right: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    BitOp {
        op: BitOp,
        left: Box<Spanned<Expr<'src>>>,
        right: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Call {
        name: Box<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Subscr {
        expr: Box<Spanned<Expr<'src>>>,
        index: Box<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Assign {
        left: Box<Spanned<Expr<'src>>>,
        right: Box<Spanned<Expr<'src>>>,
    },
    Make {
        ty: Type<'src>,
        expr: Option<Box<Spanned<Expr<'src>>>>,
    },
    Tuple {
        exprs: Vec<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    ChanRead {
        name: &'src str,
        ty: Option<Type<'src>>,
    },
    List {
        exprs: Vec<Spanned<Expr<'src>>>,
        ty: Option<Type<'src>>,
    },
    Cast {
        expr: Box<Spanned<Expr<'src>>>,
        ty: Type<'src>,
    },
    Ref {
        expr: Box<Expr<'src>>,
        ty: Option<Type<'src>>,
    },
    Deref {
        expr: Box<Expr<'src>>,
        ty: Option<Type<'src>>,
    },
    Is {
        expr: Box<Spanned<Expr<'src>>>,
        ix: Option<usize>,
        ty: Type<'src>,
    },
}

impl<'src> Expr<'src> {
    pub fn get_type(&self) -> Type<'src> {
        match self {
            Expr::Literal { ty, .. } => ty.clone(),
            Expr::Variable { ty, .. } => ty.clone().unwrap(),
            Expr::Neg { ty, .. } => ty.clone().unwrap(),
            Expr::Not { ty, .. } => ty.clone().unwrap(),
            Expr::BitNot { ty, .. } => ty.clone().unwrap(),
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
            Expr::Deref { expr: _, ty } => ty.clone().unwrap(),
            Expr::Is { .. } => Type::Bool,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'src> {
    Expr(Box<Spanned<Expr<'src>>>),
    Decl(Spanned<Expr<'src>>, Box<Spanned<Expr<'src>>>),
    For(&'src str, Box<Spanned<Expr<'src>>>, Vec<Spanned<Stmt<'src>>>),
    While(Box<Spanned<Expr<'src>>>, Vec<Spanned<Stmt<'src>>>),
    IfElse(
        Box<Spanned<Expr<'src>>>,
        Vec<Spanned<Stmt<'src>>>,
        Option<Vec<Spanned<Stmt<'src>>>>,
    ),
    Return(Box<Option<Spanned<Expr<'src>>>>),
    Coroutine(Spanned<Expr<'src>>),
    ChanWrite(&'src str, Box<Spanned<Expr<'src>>>),
    Block(Vec<Spanned<Stmt<'src>>>),
    Continue,
    Break,
}

pub type Spanned<T> = (usize, T, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'src> {
    pub name: &'src str,
    pub ty: Type<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func<'src> {
    pub name: &'src str,
    pub args: Vec<Param<'src>>,
    pub body: Vec<Spanned<Stmt<'src>>>,
    pub retty: Type<'src>,
    pub externed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel<'src> {
    Func(Func<'src>),
    Typedef(&'src str, Type<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'src> {
    pub top_levels: Vec<TopLevel<'src>>,
}

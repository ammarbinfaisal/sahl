use crate::syntax::{Expr, Lit, Type};

pub fn extract_var_name<'src>(var: &Expr<'src>) -> Option<&'src str> {
    match var {
        Expr::Variable { name, ty: _ } => Some(name),
        _ => None,
    }
}

pub fn get_literal_type(lit: &Lit) -> Type<'static> {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Double(_) => Type::Double,
        Lit::Bool(_) => Type::Bool,
        Lit::Str(_) => Type::Str,
        Lit::Char(_) => Type::Char,
    }
}

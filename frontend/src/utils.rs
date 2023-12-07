use crate::syntax::Expr;

pub fn extract_var_name<'src>(var: &Expr<'src>) -> Option<&'src str> {
    match var {
        Expr::Variable { name, ty: _ } => Some(name),
        _ => None,
    }
}

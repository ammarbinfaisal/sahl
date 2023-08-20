use crate::syntax::Expr;

pub fn extract_var_name(var: &Expr) -> Option<String> {
    match var {
        Expr::Variable { name, ty } => Some(name.clone()),
        _ => None,
    }
}

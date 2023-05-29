// emit go code from ast

use crate::syntax::*;

// just recursively emit go code from ast

fn ty_to_go(ty: &Type) -> String {
    match ty {
        Type::Int => "int64".to_string(),
        Type::Double => "float64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Str => "string".to_string(),
        Type::Void => "".to_string(),
        Type::Char => "byte".to_string(),
        Type::List(ty) => {
            let ty = ty_to_go(ty);
            format!("[]{}", ty)
        }
        Type::Tuple(tys) => {
            let mut s = String::new();
            s.push_str("(");
            for (i, ty) in tys.iter().enumerate() {
                s.push_str(&ty_to_go(ty));
                if i < tys.len() - 1 {
                    s.push_str(", ");
                }
            }
            s.push_str(")");
            s
        }
        Type::Map(ty1, ty2) => {
            let ty1 = ty_to_go(ty1);
            let ty2 = ty_to_go(ty2);
            format!("map[{}]{}", ty1, ty2)
        }
        _ => unreachable!("ty_to_go: {:?}", ty),
    }
}

fn arithop_to_go(op: &ArithOp) -> &'static str {
    match op {
        ArithOp::Add => "+",
        ArithOp::Sub => "-",
        ArithOp::Mul => "*",
        ArithOp::Div => "/",
        ArithOp::Mod => "%",
    }
}

fn boolop_to_go(op: &BoolOp) -> &'static str {
    match op {
        BoolOp::And => "&&",
        BoolOp::Or => "||",
    }
}

fn cmpop_to_go(op: &CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "==",
        CmpOp::Ne => "!=",
        CmpOp::Lt => "<",
        CmpOp::Gt => ">",
        CmpOp::Le => "<=",
        CmpOp::Ge => ">=",
    }
}

fn bitop_to_go(op: &BitOp) -> &'static str {
    match op {
        BitOp::And => "&",
        BitOp::Or => "|",
        BitOp::Xor => "^",
        BitOp::Shl => "<<",
        BitOp::Shr => ">>",
    }
}

fn unescape(s: &str) -> String {
    let mut s = s.to_string();
    s = s.replace("\n", "\\n");
    s = s.replace("\t", "\\t");
    s = s.replace("\r", "\\r");
    s = s.replace("\"", "\\\"");
    s
}

fn compile_expr(expr: &Expr) -> String {
    match expr {
        Expr::Literal { lit, ty: _ } => {
            let mut code = String::new();
            match lit {
                Lit::Int(i) => {
                    code.push_str(&format!("{}", i));
                }
                Lit::Double(d) => {
                    code.push_str(&format!("{}", d));
                }
                Lit::Bool(b) => {
                    code.push_str(&format!("{}", b));
                }
                Lit::Str(s) => {
                    let string = String::from_utf8_lossy(s);
                    let string = unescape(&string);
                    code.push_str(&format!("\"{}\"", string));
                }
                Lit::Char(c) => {
                    code.push_str(&format!("'{}'", *c as char));
                }
            }
            code
        }
        Expr::Variable { name, ty: _ } => {
            let mut code = String::new();
            code.push_str(&name);
            code
        }
        Expr::Neg { expr, ty: _ } => {
            let mut code = String::new();
            code.push_str("-");
            code.push_str(&compile_expr(&expr.1));
            code
        }
        Expr::Arith {
            op,
            left,
            right,
            ty: _,
        } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&left.1));
            code.push_str(&format!(" {} ", arithop_to_go(op)));
            code.push_str(&compile_expr(&right.1));
            code
        }
        Expr::CmpOp {
            op,
            left,
            right,
            ty: _,
        } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&left.1));
            code.push_str(&format!(" {} ", cmpop_to_go(op)));
            code.push_str(&compile_expr(&right.1));
            code
        }
        Expr::BoolOp {
            op,
            left,
            right,
            ty: _,
        } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&left.1));
            code.push_str(&format!(" {} ", boolop_to_go(op)));
            code.push_str(&compile_expr(&right.1));
            code
        }
        Expr::Not { expr, ty: _ } => {
            let mut code = String::new();
            code.push_str("!");
            code.push_str(&compile_expr(&expr.1));
            code
        }
        Expr::BitOp {
            op,
            left,
            right,
            ty: _,
        } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&left.1));
            code.push_str(&format!(" {} ", bitop_to_go(op)));
            code.push_str(&compile_expr(&right.1));
            code
        }
        Expr::List { exprs, ty: _ } => {
            let mut code = String::new();
            code.push_str("[");
            for (i, expr) in exprs.iter().enumerate() {
                code.push_str(&compile_expr(&expr.1));
                if i < exprs.len() - 1 {
                    code.push_str(", ");
                }
            }
            code.push_str("]");
            code
        }
        Expr::Tuple { exprs, ty: _ } => {
            let mut code = String::new();
            code.push_str("(");
            for (i, expr) in exprs.iter().enumerate() {
                code.push_str(&compile_expr(&expr.1));
                if i < exprs.len() - 1 {
                    code.push_str(", ");
                }
            }
            code.push_str(")");
            code
        }
        Expr::Subscr { expr, index, ty: _ } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&expr.1));
            code.push_str("[");
            code.push_str(&compile_expr(&index.1));
            code.push_str("]");
            code
        }
        Expr::Call { name, args, ty: _ } => {
            let mut code = String::new();
            if name == "append" {
                let arg = compile_expr(&args[0].1);
                code.push_str(&format!(
                    "{} = append({}, {})",
                    arg,
                    arg,
                    compile_expr(&args[1].1)
                ));
                return code;
            }
            if name == "print" {
                code.push_str("fmt.Printf(");
                let arg_format = Vec::from_iter(args.iter().map(|a| {
                    match a.1.get_type() {
                        Type::Int => "%d",
                        Type::Double => "%f",
                        Type::Bool => "%t",
                        Type::Str => "%s",
                        Type::Char => "%c",
                        _ => "%v",
                    }
                }));
                let arg_format = arg_format.join("");
                code.push_str(&format!("\"{}\", ", arg_format));
                for (i, arg) in args.iter().enumerate() {
                    code.push_str(&compile_expr(&arg.1));
                    if i < args.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str(")");
            } else {
            code.push_str(&name);
            code.push_str("(");
            for (i, arg) in args.iter().enumerate() {
                code.push_str(&compile_expr(&arg.1));
                if i < args.len() - 1 {
                    code.push_str(", ");
                }
            }
            code.push_str(")");
            };
            code
        }
        Expr::Range { .. } => unimplemented!("range not implemented for translation to golang"),
        Expr::Assign { left, right } => {
            let mut code = String::new();
            code.push_str(&compile_expr(&left.1));
            code.push_str(" = ");
            code.push_str(&compile_expr(&right.1));
            code
        }
        Expr::Make { ty, expr: _ } => {
            let mut code = String::new();
            code.push_str(&ty_to_go(ty));
            code.push_str("{}");
            code
        }
        Expr::ChanRead { name, ty: _ } => {
            let mut code = String::new();
            code.push_str("<-");
            code.push_str(&name);
            code
        }
        Expr::Cast { expr, ty } => {
            let mut code = String::new();
            code.push_str(&ty_to_go(ty));
            code.push_str("(");
            code.push_str(&compile_expr(&expr.1));
            code.push_str(")");
            code
        }
    }
}

fn compile_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Expr(expr) => {
            let mut code = String::new();
            code.push_str(&compile_expr(&expr.1));
            code.push_str("\n");
            code
        }
        Stmt::Decl(lhs, rhs) => {
            let mut code = String::new();
            code.push_str(lhs);
            code.push_str(" := ");
            code.push_str(&ty_to_go(&rhs.1.get_type()));
            code.push_str("(");
            code.push_str(&compile_expr(&rhs.1));
            code.push_str(")\n");
            code
        }
        Stmt::IfElse(cond, then, els) => {
            let mut code = String::new();
            code.push_str(&format!("if {} {{\n", compile_expr(&cond.1)));
            for stmt in then {
                code.push_str(&compile_stmt(&stmt.1));
            }
            code.push_str("} ");
            if let Some(els) = els {
                code.push_str("else {\n");
                for stmt in els {
                    code.push_str(&compile_stmt(&stmt.1));
                }
                code.push_str("}\n");
            } else {
                code.push_str("\n");
            }
            code
        }
        Stmt::While(cond, body) => {
            let mut code = String::new();
            code.push_str(&format!("for {} {{\n", compile_expr(&cond.1)));
            for stmt in body {
                code.push_str(&compile_stmt(&stmt.1));
            }
            code.push_str("}\n");
            code
        }
        Stmt::For(var, iter, body) => {
            let mut code = String::new();
            code.push_str(&format!(
                "for _, {} := range {} {{\n",
                var,
                compile_expr(&iter.1)
            ));
            for stmt in body {
                code.push_str(&compile_stmt(&stmt.1));
            }
            code.push_str("}\n");
            code
        }
        Stmt::Return(expr) => {
            let mut code = String::new();
            code.push_str("return ");
            code.push_str(&compile_expr(&expr.1));
            code.push_str("\n");
            code
        }
        Stmt::Break => "break\n".to_string(),
        Stmt::Continue => "continue\n".to_string(),
        Stmt::ChanWrite(name, expr) => {
            let mut code = String::new();
            code.push_str(&format!("{} <- {}\n", name, compile_expr(&expr.1)));
            code
        }
        Stmt::Coroutine(expr) => {
            let mut code = String::new();
            code.push_str("go ");
            code.push_str(&compile_expr(&expr.1));
            code.push_str("\n");
            code
        }
    }
}

fn compile_func(func: &Func) -> String {
    let mut code = String::new();
    code.push_str(&format!("func {}(", func.name));
    for (i, arg) in func.args.iter().enumerate() {
        let goty = ty_to_go(&arg.ty);
        code.push_str(&format!("{} {}", arg.name, goty));
        if i < func.args.len() - 1 {
            code.push_str(", ");
        }
    }
    code.push_str(") ");
    code.push_str(&format!("{} {{\n", ty_to_go(&func.retty)));
    for stmt in &func.body {
        code.push_str(&compile_stmt(&stmt.1));
    }
    code.push_str("}\n");
    code
}

pub fn compile_program(program: &Program) -> String {
    let mut code = String::new();
    code.push_str("package main\n");
    code.push_str("import \"fmt\"\n");
    for stmt in &program.funcs {
        code.push_str(&compile_func(stmt));
    }
    code
}

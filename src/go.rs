use crate::{syntax::*, utils::extract_var_name};

// just recursively emit go code from ast

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

struct GOCodegen {
    header: String,            // struct, import, etc
    type_count: usize,         // t<type_count>
    types: Vec<(Type, usize)>, // (type, printfn)
    print_count: usize,        // _print_<print_count>
    prints: Vec<String>,
}

impl GOCodegen {
    fn find_type(&self, ty: &Type) -> Option<usize> {
        for (i, t) in self.types.iter().enumerate() {
            if t.0 == ty.clone() {
                return Some(i);
            }
        }
        None
    }

    fn find_print(&self, ty: &Type) -> Option<String> {
        for (i, t) in self.types.iter().enumerate() {
            if t.0 == ty.clone() {
                return Some(self.prints[i].clone());
            }
        }
        None
    }

    fn ty_to_go(&mut self, tyy: &Type) -> String {
        match tyy {
            Type::Int => "int64".to_string(),
            Type::Double => "float64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Str => "string".to_string(),
            Type::Void => "".to_string(),
            Type::Char => "byte".to_string(),
            Type::List(ty) => {
                let goty = self.ty_to_go(ty);
                if let Some(_) = self.find_type(ty) {
                    // cool
                } else {
                    // add a print function for this type
                    let mut print_code = String::new();
                    print_code.push_str(&format!(
                        "func _print_{}(v []{}) {{\n",
                        self.print_count, goty
                    ));
                    self.prints.push(format!("_print_{}", self.print_count));
                    self.types.push((tyy.clone(), self.print_count));
                    print_code.push_str("fmt.Print(\"[\")\n");
                    print_code.push_str("for i, vv := range v {\n");
                    match *ty.clone() {
                        Type::Int => {
                            print_code.push_str("fmt.Printf(\"%d\", vv)\n");
                        }
                        Type::Double => {
                            print_code.push_str("fmt.Printf(\"%f\", vv)\n");
                        }
                        Type::Char => {
                            print_code.push_str("fmt.Printf(\"%c\", vv)\n");
                        }
                        Type::Str => {
                            print_code.push_str("fmt.Printf(\"%s\", vv)\n");
                        }
                        Type::Bool => {
                            print_code.push_str("fmt.Printf(\"%t\", vv)\n");
                        }
                        tyy => {
                            let print_fn = self.find_print(&tyy);
                            if let Some(print_fn) = print_fn {
                                print_code.push_str(&format!("{}(vv)\n", print_fn));
                            } else {
                                let _ = self.ty_to_go(&tyy);
                                let printfn = self.find_print(&tyy).unwrap();
                                print_code.push_str(&format!("{}(vv)\n", printfn));
                            }
                        }
                    }
                    print_code.push_str("if i != len(v)-1 {\n");
                    print_code.push_str("fmt.Print(\", \")\n");
                    print_code.push_str("}\n");
                    print_code.push_str("}\n");
                    print_code.push_str("fmt.Print(\"]\")\n");
                    print_code.push_str("}\n");
                    self.header.push_str(&print_code);
                    self.print_count += 1;
                }
                format!("[]{}", goty)
            }
            Type::Tuple(tys) => {
                let exists = self.find_type(tyy);
                if let Some(i) = exists {
                    format!("t{}", i)
                } else {
                    let mut code = String::new();
                    code.push_str("struct {\n");
                    for (i, ty) in tys.iter().enumerate() {
                        let ty = self.ty_to_go(ty);
                        code.push_str(&format!("f{} {}\n", i, ty));
                    }
                    code.push_str("}");
                    self.header
                        .push_str(&format!("type t{} {}\n", self.type_count, code));
                    // add a print function for this type
                    let mut print_code = String::new();
                    print_code.push_str(&format!(
                        "func _print_{}(v t{}) {{\n",
                        self.print_count, self.type_count
                    ));
                    print_code.push_str("fmt.Print(\"(\")\n");
                    for (i, ty) in tys.iter().enumerate() {
                        match ty {
                            Type::Int => {
                                print_code.push_str(&format!("fmt.Printf(\"%d\", v.f{})\n", i));
                            }
                            Type::Double => {
                                print_code.push_str(&format!("fmt.Printf(\"%f\", v.f{})\n", i));
                            }
                            Type::Char => {
                                print_code.push_str(&format!("fmt.Printf(\"%c\", v.f{})\n", i));
                            }
                            Type::Str => {
                                print_code.push_str(&format!("fmt.Printf(\"%s\", v.f{})\n", i));
                            }
                            _ => {
                                print_code.push_str(&format!("fmt.Printf(%v, v.f{})\n", i));
                            }
                        }
                        if i != tys.len() - 1 {
                            print_code.push_str("fmt.Print(\", \")\n");
                        }
                    }
                    print_code.push_str("fmt.Print(\")\")\n");
                    print_code.push_str("}\n");
                    self.header.push_str(&print_code);
                    self.types.push((tyy.clone(), self.print_count));
                    self.prints.push(format!("_print_{}", self.print_count));
                    self.print_count += 1;
                    self.type_count += 1;
                    format!("t{}", self.type_count - 1)
                }
            }
            Type::Map(ty1, ty2) => {
                let ty1 = self.ty_to_go(ty1);
                let ty2 = self.ty_to_go(ty2);
                format!("map[{}]{}", ty1, ty2)
            }
            _ => unreachable!("ty_to_go: {:?}", tyy),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> String {
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
                code.push_str(&self.compile_expr(&expr.1));
                code
            }
            Expr::Arith {
                op,
                left,
                right,
                ty: _,
            } => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&left.1));
                code.push_str(&format!(" {} ", arithop_to_go(op)));
                code.push_str(&self.compile_expr(&right.1));
                code
            }
            Expr::CmpOp {
                op,
                left,
                right,
                ty: _,
            } => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&left.1));
                code.push_str(&format!(" {} ", cmpop_to_go(op)));
                code.push_str(&self.compile_expr(&right.1));
                code
            }
            Expr::BoolOp {
                op,
                left,
                right,
                ty: _,
            } => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&left.1));
                code.push_str(&format!(" {} ", boolop_to_go(op)));
                code.push_str(&self.compile_expr(&right.1));
                code
            }
            Expr::Not { expr, ty: _ } => {
                let mut code = String::new();
                code.push_str("!");
                code.push_str(&self.compile_expr(&expr.1));
                code
            }
            Expr::BitOp {
                op,
                left,
                right,
                ty: _,
            } => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&left.1));
                code.push_str(&format!(" {} ", bitop_to_go(op)));
                code.push_str(&self.compile_expr(&right.1));
                code
            }
            Expr::List { exprs, ty: _ } => {
                let mut code = String::new();
                code.push_str("[");
                for (i, expr) in exprs.iter().enumerate() {
                    code.push_str(&self.compile_expr(&expr.1));
                    if i < exprs.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("]");
                code
            }
            Expr::Tuple { exprs, ty } => {
                let mut code = String::new();
                let goty = self.ty_to_go(&ty.clone().unwrap());
                code.push_str(&format!("{}{{", goty));
                for (i, expr) in exprs.iter().enumerate() {
                    code.push_str(&format!("f{}: {}", i, self.compile_expr(&expr.1)));
                    if i < exprs.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("}");
                code
            }
            Expr::Subscr { expr, index, ty: _ } => {
                let mut code = String::new();
                if let Type::Tuple(_) = expr.1.get_type().clone() {
                    code.push_str(&self.compile_expr(&expr.1));
                    code.push_str(".");
                    if let Expr::Literal { lit, ty: _ } = &index.1 {
                        if let Lit::Int(i) = lit {
                            code.push_str(&format!("f{}", i));
                        }
                    } else {
                        unreachable!("tuple index must be a literal");
                    }
                } else {
                    code.push_str(&self.compile_expr(&expr.1));
                    code.push_str("[");
                    code.push_str(&self.compile_expr(&index.1));
                    code.push_str("]");
                }
                code
            }
            Expr::Call { name, args, ty } => {
                let mut code = String::new();
                let name = extract_var_name(name);
                if name.is_none() {
                    unimplemented!("Cannot compile call using a complex expression");
                }
                let name = name.unwrap();
                if name == "append" {
                    let arg = self.compile_expr(&args[0].1);
                    code.push_str(&format!(
                        "{} = append({}, {})",
                        arg,
                        arg,
                        self.compile_expr(&args[1].1)
                    ));
                    return code;
                }
                if name == "print" {
                    let arg_format = Vec::from_iter(args.iter().map(|a| match a.1.get_type() {
                        Type::Int => "%d",
                        Type::Double => "%f",
                        Type::Bool => "%t",
                        Type::Str => "%s",
                        Type::Char => "%c",
                        _ => "%v",
                    }));
                    for (i, arg) in args.iter().enumerate() {
                        match arg.1.get_type() {
                            Type::Int => {}
                            Type::Double => {}
                            Type::Bool => {}
                            Type::Str => {}
                            Type::Char => {}
                            ty => {
                                let mut printfn = self.find_print(&ty);
                                if printfn.is_none() {
                                    self.ty_to_go(&ty);
                                    printfn = Some(self.prints[self.print_count - 1].clone())
                                }
                                let printfn = printfn.unwrap();
                                code.push_str(&format!(
                                    "{}({})\n",
                                    printfn,
                                    self.compile_expr(&arg.1)
                                ));
                                continue;
                            }
                        }
                        code.push_str(&format!(
                            "fmt.Printf(\"{}\", {})\n",
                            arg_format[i],
                            self.compile_expr(&arg.1)
                        ));
                    }
                } else {
                    // wrap in return type
                    let goty = self.ty_to_go(&ty.clone().unwrap());
                    code.push_str(&format!("{}(", goty));
                    code.push_str(&name);
                    code.push_str("(");
                    for (i, arg) in args.iter().enumerate() {
                        code.push_str(&self.compile_expr(&arg.1));
                        if i < args.len() - 1 {
                            code.push_str(", ");
                        }
                    }
                    code.push_str("))");
                };
                code
            }
            Expr::Range { .. } => unimplemented!("range not implemented for translation to golang"),
            Expr::Assign { left, right } => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&left.1));
                code.push_str(" = ");
                code.push_str(&self.compile_expr(&right.1));
                code
            }
            Expr::Make { ty, expr } => {
                let mut code = String::new();
                if let Some(ex) = expr {
                    code.push_str("make(");
                    code.push_str(&self.ty_to_go(ty));
                    code.push_str(", ");
                    code.push_str(&self.compile_expr(&ex.1));
                    code.push_str(")");
                } else {
                    code.push_str(&self.ty_to_go(ty));
                    code.push_str("{}");
                }
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
                code.push_str(&self.ty_to_go(ty));
                code.push_str("(");
                code.push_str(&self.compile_expr(&expr.1));
                code.push_str(")");
                code
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expr(expr) => {
                let mut code = String::new();
                code.push_str(&self.compile_expr(&expr.1));
                code.push_str("\n");
                code
            }
            Stmt::Decl(lhs, rhs) => {
                let mut code = String::new();
                code.push_str(lhs);
                code.push_str(" := ");
                code.push_str(&self.ty_to_go(&rhs.1.get_type()));
                code.push_str("(");
                code.push_str(&self.compile_expr(&rhs.1));
                code.push_str(")\n");
                code
            }
            Stmt::IfElse(cond, then, els) => {
                let mut code = String::new();
                code.push_str(&format!("if {} {{\n", self.compile_expr(&cond.1)));
                for stmt in then {
                    code.push_str(&self.compile_stmt(&stmt.1));
                }
                code.push_str("} ");
                if let Some(els) = els {
                    code.push_str("else {\n");
                    for stmt in els {
                        code.push_str(&self.compile_stmt(&stmt.1));
                    }
                    code.push_str("}\n");
                } else {
                    code.push_str("\n");
                }
                code
            }
            Stmt::While(cond, body) => {
                let mut code = String::new();
                code.push_str(&format!("for {} {{\n", self.compile_expr(&cond.1)));
                for stmt in body {
                    code.push_str(&self.compile_stmt(&stmt.1));
                }
                code.push_str("}\n");
                code
            }
            Stmt::For(var, iter, body) => {
                let mut code = String::new();
                code.push_str(&format!(
                    "for _, {} := range {} {{\n",
                    var,
                    self.compile_expr(&iter.1)
                ));
                for stmt in body {
                    code.push_str(&self.compile_stmt(&stmt.1));
                }
                code.push_str("}\n");
                code
            }
            Stmt::Return(expr) => {
                let mut code = String::new();
                code.push_str("return ");
                code.push_str(&self.compile_expr(&expr.1));
                code.push_str("\n");
                code
            }
            Stmt::Break => "break\n".to_string(),
            Stmt::Continue => "continue\n".to_string(),
            Stmt::ChanWrite(name, expr) => {
                let mut code = String::new();
                code.push_str(&format!("{} <- {}\n", name, self.compile_expr(&expr.1)));
                code
            }
            Stmt::Coroutine(expr) => {
                let mut code = String::new();
                code.push_str("go ");
                code.push_str(&self.compile_expr(&expr.1));
                code.push_str("\n");
                code
            }
            Stmt::Block(stmts) => {
                let mut code = String::new();
                code.push_str("{\n");
                for stmt in stmts {
                    code.push_str(&self.compile_stmt(&stmt.1));
                }
                code.push_str("}\n");
                code
            }
        }
    }

    fn compile_func(&mut self, func: &Func) -> String {
        let mut code = String::new();
        code.push_str(&format!("func {}(", func.name));
        for (i, arg) in func.args.iter().enumerate() {
            let goty = self.ty_to_go(&arg.ty);
            code.push_str(&format!("{} {}", arg.name, goty));
            if i < func.args.len() - 1 {
                code.push_str(", ");
            }
        }
        code.push_str(") ");
        code.push_str(&format!("{} {{\n", self.ty_to_go(&func.retty)));
        for stmt in &func.body {
            code.push_str(&self.compile_stmt(&stmt.1));
        }
        code.push_str("}\n");
        code
    }
}

pub fn compile_program(program: &Program) -> String {
    let mut code = String::new();
    let mut gen = GOCodegen {
        header: String::new(),
        type_count: 0,
        types: Vec::new(),
        print_count: 0,
        prints: Vec::new(),
    };
    for stmt in &program.funcs {
        code.push_str(&gen.compile_func(stmt));
    }
    let mut res = String::new();
    res.push_str("package main\n");
    res.push_str("import \"fmt\"\n");
    res.push_str(&gen.header);
    res.push_str(&code);
    res
}

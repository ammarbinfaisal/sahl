use crate::syntax::*;
extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::space0,
    combinator::{map, opt},
    error::ErrorKind,
    multi::{many0, many1},
    sequence::{delimited, tuple},
    Err, IResult,
};
use std::str;

fn whitespace(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace() || c == '\n' || c == '\r')(input)
}

fn typee(source: &str) -> IResult<&str, Type> {
    let (source, ty) = alt((
        listty,
        map(tag("int"), |_| Type::Int),
        map(tag("bool"), |_| Type::Bool),
        map(tag("string"), |_| Type::Str),
        map(tag("char"), |_| Type::Char),
        chanty,
    ))(source)?;
    Ok((source, ty))
}

fn chanty(source: &str) -> IResult<&str, Type> {
    let (source, ty) = alt((
        map(tag("chan int"), |_| Type::Chan(Box::new(Type::Int))),
        map(tag("chan bool"), |_| Type::Chan(Box::new(Type::Bool))),
        map(tag("chan string"), |_| Type::Chan(Box::new(Type::Str))),
        map(tag("chan char"), |_| Type::Chan(Box::new(Type::Char))),
    ))(source)?;
    Ok((source, ty))
}

fn listty(source: &str) -> IResult<&str, Type> {
    let (source, _) = tag("[")(source)?;
    let (source, ty) = typee(source)?;
    let (source, _) = tag("]")(source)?;
    Ok((source, Type::List(Box::new(ty))))
}

fn isreserved(s: &str) -> bool {
    match s {
        "let" | "in" | "if" | "then" | "else" | "true" | "false" | "fun" | "return" | "make" => {
            true
        }
        _ => false,
    }
}

fn identifier(input: &str) -> IResult<&str, String> {
    let (input, id) = take_while_m_n(1, 32, |c: char| c.is_alphabetic() || c == '_')(input)?;
    if isreserved(id) {
        Err(Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Fail,
        }))
    } else {
        Ok((input, id.to_string()))
    }
}

fn string(source: &str) -> IResult<&str, Lit> {
    map(
        delimited(tag("\""), take_while(|c: char| c != '"'), tag("\"")),
        |s: &str| Lit::Str(s.as_bytes().to_vec()),
    )(source)
}

fn charr(source: &str) -> IResult<&str, Lit> {
    map(
        delimited(
            tag("'"),
            take_while_m_n(1, 1, |c: char| c != '\''),
            tag("'"),
        ),
        |s: &str| Lit::Char(s.as_bytes()[0]),
    )(source)
}

fn natural(source: &str) -> IResult<&str, Lit> {
    let (source, nat) = take_while_m_n(1, 19, |c: char| c.is_digit(10))(source)?;
    Ok((source, Lit::Int(nat.parse::<i64>().unwrap())))
}

fn boolean(source: &str) -> IResult<&str, Lit> {
    map(alt((tag("true"), tag("false"))), |s: &str| {
        Lit::Bool(s == "true")
    })(source)
}

fn subscript(source: &str) -> IResult<&str, Expr> {
    let (source, name) = identifier(source)?;
    let (source, subscrs) = many0(map(
        tuple((tag("["), expression, tag("]"))),
        |(_, expr, _)| expr,
    ))(source)?;
    let mut res = Expr::Variable(name);
    for subscr in subscrs {
        res = Expr::Subscr(Box::new(res), Box::new(subscr));
    }
    Ok((source, res))
}

fn call(source: &str) -> IResult<&str, Expr> {
    let (source, name) = identifier(source)?;
    let (source, _) = tag("(")(source)?;
    let (source, arg) = expression(source)?;
    let (source, args) = many0(map(
        tuple((tag(","), space0, expression, space0)),
        |(_, _, e, _)| e,
    ))(source)?;
    let (source, _) = tag(")")(source)?;
    let args = vec![arg].into_iter().chain(args).collect();
    Ok((source, Expr::Call(name, args)))
}

fn variable(source: &str) -> IResult<&str, Expr> {
    let (source, name) = identifier(source)?;
    Ok((source, Expr::Variable(name)))
}

fn factor(source: &str) -> IResult<&str, Expr> {
    let (source, unop) = opt(alt((tag("-"), tag("!"))))(source)?;
    let (source, t1) = alt((
        map(alt((string, charr, natural, boolean)), Expr::Literal),
        call,
        subscript,
        variable,
    ))(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, alt((tag("*"), tag("/"), tag("%"))), space0),
        alt((call, variable, map(natural, Expr::Literal))),
    )))(source)?;
    let mut res = match unop {
        Some(u) => {
            let res = match u {
                "-" => Expr::Neg(Box::new(t1)),
                "!" => Expr::Not(Box::new(t1)),
                _ => panic!("unexpected unop"),
            };
            res
        }
        None => t1,
    };
    for (op, lit) in exs {
        res = match op {
            "*" => Expr::Arith(ArithOp::Mul, Box::new(res), Box::new(lit)),
            "/" => Expr::Arith(ArithOp::Div, Box::new(res), Box::new(lit)),
            "%" => Expr::Arith(ArithOp::Mod, Box::new(res), Box::new(lit)),
            _ => panic!("unexpected operator"),
        };
    }
    Ok((source, res))
}

fn term(source: &str) -> IResult<&str, Expr> {
    let (source, t1) = factor(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, alt((tag("+"), tag("-"))), space0),
        factor,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        res = match op {
            "+" => Expr::Arith(ArithOp::Add, Box::new(res), Box::new(lit)),
            "-" => Expr::Arith(ArithOp::Sub, Box::new(res), Box::new(lit)),
            _ => panic!("unexpected operator"),
        };
    }
    Ok((source, res))
}

fn comparision(source: &str) -> IResult<&str, Expr> {
    let (source, t1) = term(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            space0,
            alt((
                tag("<"),
                tag(">"),
                tag("<="),
                tag(">="),
                tag("=="),
                tag("!="),
            )),
            space0,
        ),
        term,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        res = match op {
            "<" => Expr::CmpOp(CmpOp::Lt, Box::new(res), Box::new(lit)),
            ">" => Expr::CmpOp(CmpOp::Gt, Box::new(res), Box::new(lit)),
            "<=" => Expr::CmpOp(CmpOp::Le, Box::new(res), Box::new(lit)),
            ">=" => Expr::CmpOp(CmpOp::Ge, Box::new(res), Box::new(lit)),
            "==" => Expr::CmpOp(CmpOp::Eq, Box::new(res), Box::new(lit)),
            "!=" => Expr::CmpOp(CmpOp::Ne, Box::new(res), Box::new(lit)),
            _ => panic!("unexpected operator"),
        };
    }
    Ok((source, res))
}

pub fn aexp(source: &str) -> IResult<&str, Expr> {
    let (source, t1) = comparision(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, alt((tag("&&"), tag("||"))), space0),
        comparision,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        res = match op {
            "&&" => Expr::BoolOp(BoolOp::And, Box::new(res), Box::new(lit)),
            "||" => Expr::BoolOp(BoolOp::Or, Box::new(res), Box::new(lit)),
            _ => panic!("unexpected operator"),
        };
    }
    Ok((source, res))
}

fn chanread(source: &str) -> IResult<&str, Expr> {
    let (source, _) = tag("<-")(source)?;
    let (source, id) = identifier(source)?;
    Ok((source, Expr::ChanRead(id)))
}

fn assignment(source: &str) -> IResult<&str, Expr> {
    let (source, lhs) = alt((variable, subscript))(source)?;
    let (source, _) = delimited(space0, tag("="), space0)(source)?;
    let (source, expr) = alt((chanread, aexp))(source)?;
    Ok((source, Expr::Assign(Box::new(lhs), Box::new(expr))))
}

fn make(source: &str) -> IResult<&str, Expr> {
    let (source, _) = tag("make")(source)?;
    let (source, _) = tag("(")(source)?;
    let (source, ty) = delimited(space0, alt((listty, chanty)), space0)(source)?;
    let (source, len) = opt(map(
        tuple((
            delimited(space0, tag(","), space0),
            delimited(
                space0,
                take_while_m_n(1, 19, |c: char| c.is_digit(10)),
                space0,
            ),
        )),
        |(_, l)| l,
    ))(source)?;
    let len = match len {
        Some(l) => l.parse::<usize>().unwrap(),
        None => 0,
    };
    let (source, _) = tag(")")(source)?;
    Ok((source, Expr::Make(ty, len)))
}

pub fn expression(source: &str) -> IResult<&str, Expr> {
    alt((chanread, assignment, aexp, make, list))(source)
}

fn list(source: &str) -> IResult<&str, Expr> {
    let (source, _) = tag("[")(source)?;
    let (source, args) = many1(map(
        tuple((expression, delimited(space0, tag(","), space0))),
        |(e, _)| e,
    ))(source)?;
    let (source, _) = tag("]")(source)?;
    Ok((source, Expr::Literal(Lit::List(args))))
}

fn declaration(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("let"), space0)(source)?;
    let (source, name) = identifier(source)?;
    let (source, _) = delimited(space0, tag("="), space0)(source)?;
    let (source, value) = expression(source)?;
    Ok((source, Stmt::Decl(name.to_string(), Box::new(value))))
}

fn forin(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("for"), space0)(source)?;
    let (source, id) = identifier(source)?;
    let (source, _) = delimited(space0, tag("in"), space0)(source)?;
    let (source, ex2) = expression(source)?;
    let (source, stmts) = block(source)?;
    Ok((source, Stmt::For(id, Box::new(ex2), stmts)))
}

fn block(source: &str) -> IResult<&str, Vec<Stmt>> {
    let (source, _) = delimited(space0, tag("{"), whitespace)(source)?;
    let (source, stmts) = many0(delimited(whitespace, statement, whitespace))(source)?;
    let (source, _) = delimited(whitespace, tag("}"), space0)(source)?;
    Ok((source, stmts))
}

fn ifelse(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("if"), space0)(source)?;
    let (source, ex1) = expression(source)?;
    let (source, thenst) = block(source)?;
    let (source, _) = opt(delimited(space0, tag("else"), space0))(source)?;
    let (source, elseif) = opt(ifelse)(source)?;
    match elseif {
        Some(elseif) => Ok((
            source,
            Stmt::IfElse(Box::new(ex1), thenst, Some(vec![elseif])),
        )),
        None => {
            let (source, elsest) = opt(block)(source)?;
            Ok((source, Stmt::IfElse(Box::new(ex1), thenst, elsest)))
        }
    }
}

fn whileloop(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("while"), space0)(source)?;
    let (source, ex1) = delimited(space0, expression, space0)(source)?;
    let (source, stmts) = block(source)?;
    Ok((source, Stmt::While(Box::new(ex1), stmts)))
}

fn comment(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = tag("#")(source)?;
    let (source, _) = take_while(|c| c != '\n')(source)?;
    Ok((source, Stmt::Comment))
}

fn breakst(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("break"), space0)(source)?;
    Ok((source, Stmt::Break))
}

fn continuest(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("continue"), space0)(source)?;
    Ok((source, Stmt::Continue))
}

fn returnst(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("return"), space0)(source)?;
    let (source, ex) = expression(source)?;
    Ok((source, Stmt::Return(Box::new(ex))))
}

fn coroutine(source: &str) -> IResult<&str, Stmt> {
    let (source, _) = delimited(space0, tag("sahl"), space0)(source)?;
    let (source, fncall) = call(source)?;
    Ok((source, Stmt::Coroutine(fncall)))
}

fn chanwrite(source: &str) -> IResult<&str, Stmt> {
    let (source, id) = identifier(source)?;
    let (source, _) = delimited(space0, tag("<-"), space0)(source)?;
    let (source, ex) = expression(source)?;
    Ok((source, Stmt::ChanWrite(id, Box::new(ex))))
}

fn statement(source: &str) -> IResult<&str, Stmt> {
    let (source, stmt) = alt((
        chanwrite,
        coroutine,
        forin,
        ifelse,
        whileloop,
        declaration,
        returnst,
        breakst,
        continuest,
        comment,
        map(expression, |ex| Stmt::Expr(Box::new(ex))),
    ))(source)?;
    Ok((source, stmt))
}

fn parameter(source: &str) -> IResult<&str, Param> {
    let (source, name) = identifier(source)?;
    let (source, _) = delimited(space0, tag(":"), space0)(source)?;
    let (source, ty) = typee(source)?;
    Ok((source, Param { name, ty }))
}

fn function(source: &str) -> IResult<&str, Func> {
    let (source, _) = delimited(space0, tag("fun"), space0)(source)?;
    let (source, name) = identifier(source)?;
    let (source, _) = delimited(space0, tag("("), space0)(source)?;
    let (source, arg) = opt(parameter)(source)?;
    let mut args = vec![];
    let source = if arg.is_some() {
        let (source, args2) = many0(map(
            tuple((delimited(space0, tag(","), space0), parameter)),
            |(_, p)| p,
        ))(source)?;
        let (source, _) = delimited(space0, tag(")"), space0)(source)?;
        args = vec![arg.unwrap()].into_iter().chain(args2).collect();
        source
    } else {
        source
    };
    let (source, retty) = opt(map(
        tuple((delimited(space0, tag("->"), space0), typee)),
        |(_, ty)| ty,
    ))(source)?;
    let (source, body) = block(source)?;
    Ok((
        source,
        Func {
            name,
            args,
            body,
            retty: retty.unwrap_or(Type::Void),
        },
    ))
}

fn mainfn(source: &str) -> IResult<&str, Vec<Stmt>> {
    let (source, _) = delimited(space0, tag("fun"), space0)(source)?;
    let (source, _) = delimited(space0, tag("main"), space0)(source)?;
    let (source, _) = delimited(space0, tag("("), space0)(source)?;
    let (source, _) = delimited(space0, tag(")"), space0)(source)?;
    let (source, body) = block(source)?;
    Ok((source, body))
}

pub fn program(source: &str) -> IResult<&str, Program> {
    let (source, funcs) = many0(delimited(whitespace, function, whitespace))(source)?;
    let (source, mainfunc) = delimited(whitespace, mainfn, whitespace)(source)?;
    Ok((
        source,
        Program {
            funcs,
            main: mainfunc,
        },
    ))
}

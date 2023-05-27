use crate::syntax::*;
extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::{none_of, space0},
    combinator::{map, opt},
    error::{context, ContextError, Error, ErrorKind, ParseError},
    multi::{many0, many1},
    number::complete::float,
    sequence::{delimited, tuple},
    Err, IResult,
};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering};

static IDX: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct ErrorPos<'a> {
    pub idx: usize,
    pub error: ErrorParse<'a>,
    pub ctx: &'static str,
}

impl<'a> ParseError<&'a str> for ErrorPos<'a> {
    fn from_error_kind(input: &'a str, kind: ErrorKind) -> Self {
        match kind {
            ErrorKind::Tag => ErrorPos {
                idx: IDX.load(Ordering::SeqCst),
                error: ErrorParse::Expected(input),
                ctx: "",
            },
            _ => ErrorPos {
                idx: IDX.load(Ordering::SeqCst),
                error: ErrorParse::Unconsumed(input),
                ctx: "",
            },
        }
    }

    fn append(_: &'a str, _: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a> ContextError<&'a str> for ErrorPos<'a> {
    fn add_context(_: &'a str, ctx: &'static str, other: Self) -> Self {
        ErrorPos { ctx, ..other }
    }
}

#[derive(Debug)]
pub enum ErrorParse<'a> {
    Expected(&'a str),
    Unconsumed(&'a str),
}

fn whitespace<'a>(input: &'a str) -> IResult<&'a str, &str, ErrorPos<'a>> {
    let (source, consumed) =
        take_while(|c: char| c.is_whitespace() || c == '\n' || c == '\r')(input)?;
    IDX.fetch_add(consumed.len(), Ordering::SeqCst);
    Ok((source, source))
}

fn comment<'a>(source: &'a str) -> IResult<&'a str, &str, ErrorPos<'a>> {
    let (mut source, _) = whitespace(source)?;
    while source.starts_with("//") {
        (source, _) = spantag("//")(source)?;
        (source, _) = take_while(|c| c != '\n')(source)?;        
    }
    Ok((source, ""))
}

fn faaaltu<'a>(source: &'a str) -> IResult<&'a str, &'a str, ErrorPos> {
    let (source, consumeed) = alt((whitespace, comment))(source)?;
    Ok((source, consumeed))
}

fn span_space0<'a>(input: &'a str) -> IResult<&'a str, &str, ErrorPos<'a>> {
    let (source, consumed) = space0(input)?;
    IDX.fetch_add(consumed.len(), Ordering::SeqCst);
    Ok((source, consumed))
}

fn spantag<'a>(s: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, ErrorPos<'a>> {
    move |input: &'a str| {
        let res = tag::<_, _, Error<_>>(s)(input);
        match res {
            Ok((source, consumed)) => {
                IDX.fetch_add(consumed.len(), Ordering::SeqCst);
                Ok((source, consumed))
            }
            Err(_) => Err(Err::Error(ErrorPos {
                idx: IDX.load(Ordering::SeqCst),
                error: ErrorParse::Expected(s),
                ctx: "",
            })),
        }
    }
}

fn typee<'a>(source: &'a str) -> IResult<&'a str, Type, ErrorPos<'a>> {
    let (source, ty) = alt::<_, _, ErrorPos, _>((
        listty,
        tuplety,
        map(spantag("int"), |_| Type::Int),
        map(spantag("bool"), |_| Type::Bool),
        map(spantag("string"), |_| Type::Str),
        map(spantag("char"), |_| Type::Char),
        map(spantag("double"), |_| Type::Double),
        chanty,
        mapty,
    ))(source)?;
    Ok((source, ty))
}

fn mapty<'a>(source: &'a str) -> IResult<&'a str, Type, ErrorPos<'a>> {
    let (source, _) = spantag("map")(source)?;
    let (source, _) = spantag("<")(source)?;
    let (source, _) = span_space0(source)?;
    let (source, key) = typee(source)?;
    let (source, _) = spantag(",")(source)?;
    let (source, _) = span_space0(source)?;
    let (source, val) = typee(source)?;
    let (source, _) = span_space0(source)?;
    let (source, _) = spantag(">")(source)?;
    Ok((source, Type::Map(Box::new(key), Box::new(val))))
}

fn chanty<'a>(source: &'a str) -> IResult<&'a str, Type, ErrorPos<'a>> {
    let (source, _) = spantag("chan<")(source)?;
    let (source, _) = span_space0(source)?;
    let (source, ty) = typee(source)?;
    let (source, _) = span_space0(source)?;
    let (source, _) = spantag(">")(source)?;
    Ok((source, Type::Chan(Box::new(ty))))
}

fn listty<'a>(source: &'a str) -> IResult<&'a str, Type, ErrorPos<'a>> {
    let (source, _) = spantag("[")(source)?;
    let (source, ty) = alt((listty, typee))(source)?;
    let (source, _) = spantag("]")(source)?;
    Ok((source, Type::List(Box::new(ty))))
}

fn tuplety<'a>(source: &'a str) -> IResult<&'a str, Type, ErrorPos<'a>> {
    let (source, _) = spantag("(")(source)?;
    let (source, mut tys) = many0(map(
        tuple((span_space0, typee, span_space0, spantag(","))),
        |(_, e, _, _)| e,
    ))(source)?;
    let (source, last) = opt(map(tuple((span_space0, typee, span_space0)), |(_, e, _)| e))(source)?;
    let (source, _) = spantag(")")(source)?;
    match last {
        Some(e) => {
            tys.push(e);
        }
        None => {}
    };
    Ok((source, Type::Tuple(tys)))
}

fn isreserved<'a>(s: &'a str) -> bool {
    match s {
        "let" | "in" | "if" | "then" | "else" | "true" | "false" | "fun" | "return" | "make" => {
            true
        }
        _ => false,
    }
}

fn identifier<'a>(input: &'a str) -> IResult<&'a str, String, ErrorPos<'a>> {
    let (input, id1) = take_while_m_n(1, 1, |c: char| c.is_alphabetic() || c == '_')(input)?;
    let (input, id2) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    let id = format!("{}{}", id1, id2);
    if isreserved(id.as_str()) {
        Err(Err::Error(ErrorPos::from_error_kind(input, ErrorKind::Tag)))
    } else {
        Ok((input, id))
    }
}

fn parse_escape<'a>(source: &'a str) -> IResult<&'a str, u8, ErrorPos<'a>> {
    let (source, _) = spantag("\\")(source)?;
    let (source, esc) = alt((
        map(spantag("n"), |_| '\n'),
        map(spantag("r"), |_| '\r'),
        map(spantag("t"), |_| '\t'),
        map(spantag("\\"), |_| '\\'),
    ))(source)?;
    Ok((source, esc as u8))
}

fn string<'a>(source: &'a str) -> IResult<&'a str, Spanned<Lit>, ErrorPos<'a>> {
    let start = IDX.load(Ordering::Relaxed);
    let (source, _) = spantag("\"")(source)?;
    let (source, chars) = many0(alt((parse_escape, map(none_of("\""), |c| c as u8))))(source)?;
    let (source, _) = spantag("\"")(source)?;
    let end = IDX.load(Ordering::Relaxed);
    Ok((source, (start, Lit::Str(chars), end)))
}

fn charr<'a>(source: &'a str) -> IResult<&'a str, Spanned<Lit>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, c) = delimited(
        spantag("'"),
        take_while_m_n(1, 2, |c: char| c != '\''),
        spantag("'"),
    )(source)?;
    if c.as_bytes()[0] == '\\' as u8 {
        let (source, c) = parse_escape(c)?;
        let end_idx = IDX.load(Ordering::Relaxed);
        Ok((source, (start_idx, Lit::Char(c), end_idx)))
    } else {
        Ok((
            source,
            (start_idx, Lit::Char(c.as_bytes()[0]), start_idx + 1),
        ))
    }
}

fn natural<'a>(source: &'a str) -> IResult<&'a str, Spanned<Lit>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, nat) = take_while_m_n(1, 19, |c: char| c.is_digit(10))(source)?;
    if source.len() > 0 && source.as_bytes()[0] == '.' as u8 {
        return Err(Err::Error(ErrorPos::from_error_kind(
            source,
            ErrorKind::Tag,
        )));
    }
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Lit::Int(nat.parse::<i64>().unwrap()), end_idx),
    ))
}

fn floatt<'a>(source: &'a str) -> IResult<&'a str, Spanned<Lit>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, f) = float(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Lit::Double(f as f64), end_idx)))
}

fn boolean<'a>(source: &'a str) -> IResult<&'a str, Spanned<Lit>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, lit) = map(alt((spantag("true"), spantag("false"))), |s: &'a str| {
        Lit::Bool(s == "true")
    })(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, lit, end_idx)))
}

fn tuplee<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = spantag("(")(source)?;
    let (source, mut exprs) = many0(map(
        tuple((span_space0, expression, span_space0, spantag(","))),
        |(_, e, _, _)| e,
    ))(source)?;
    let (source, last) = opt(map(
        tuple((span_space0, expression, span_space0)),
        |(_, e, _)| e,
    ))(source)?;
    let (source, _) = spantag(")")(source)?;
    match last {
        Some(e) => {
            exprs.push(e);
        }
        None => {}
    };
    let end_idx = IDX.load(Ordering::Relaxed);
    let ex = Expr::Tuple { exprs, ty: None };
    Ok((source, (start_idx, ex, end_idx)))
}

fn subscript<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, subscrs) = many0(map(
        tuple((spantag("["), expression, spantag("]"))),
        |(_, expr, _)| expr,
    ))(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    let mut res = Expr::Variable { name, ty: None };
    for subscr in subscrs {
        res = Expr::Subscr {
            expr: Box::new((start_idx, res, end_idx)),
            index: Box::new(subscr),
            ty: None,
        };
    }
    Ok((source, (start_idx, res, end_idx)))
}

fn call<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = spantag("(")(source)?;
    let (source, arg) = opt(expression)(source)?;
    let (source, args2) = many0(map(
        tuple((
            spantag(","),
            span_space0,
            context("argument for function call", expression),
            span_space0,
        )),
        |(_, _, e, _)| e,
    ))(source)?;
    let (source, _) = spantag(")")(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    if let Some(arg) = arg {
        let mut args = vec![arg];
        args.extend(args2);
        Ok((
            source,
            (
                start_idx,
                Expr::Call {
                    name,
                    args,
                    ty: None,
                },
                end_idx,
            ),
        ))
    } else {
        Ok((
            source,
            (
                start_idx,
                Expr::Call {
                    name,
                    args: vec![],
                    ty: None,
                },
                end_idx,
            ),
        ))
    }
}

fn variable<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, name) = context("identifier", identifier)(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Expr::Variable { name, ty: None }, end_idx),
    ))
}

fn barcexpr<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let (source, _) = spantag("(")(source)?;
    let (source, expr) = delimited(span_space0, expression, span_space0)(source)?;
    let (source, _) = spantag(")")(source)?;
    Ok((source, expr))
}

fn factor<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, unop) = opt(alt((spantag("-"), spantag("!"))))(source)?;
    let (source, t1) = alt((
        map(alt((string, charr, natural, floatt, boolean)), |lit| {
            let l = match lit.1 {
                Lit::Str(s) => Expr::Literal {
                    lit: Lit::Str(s),
                    ty: Type::Str,
                },
                Lit::Char(c) => Expr::Literal {
                    lit: Lit::Char(c),
                    ty: Type::Char,
                },
                Lit::Int(i) => Expr::Literal {
                    lit: Lit::Int(i),
                    ty: Type::Int,
                },
                Lit::Double(f) => Expr::Literal {
                    lit: Lit::Double(f),
                    ty: Type::Double,
                },
                Lit::Bool(b) => Expr::Literal {
                    lit: Lit::Bool(b),
                    ty: Type::Bool,
                },
            };
            (lit.0, l, lit.2)
        }),
        call,
        subscript,
        variable,
        barcexpr,
    ))(source)?;
    let first_idx = IDX.load(Ordering::Relaxed);
    let (source, exs) = many0(tuple((
        delimited(
            span_space0,
            alt((spantag("*"), spantag("/"), spantag("%"))),
            span_space0,
        ),
        alt((
            call,
            subscript,
            variable,
            map(natural, |n| {
                (
                    n.0,
                    Expr::Literal {
                        lit: n.1,
                        ty: Type::Int,
                    },
                    n.2,
                )
            }),
            map(floatt, |n| {
                (
                    n.0,
                    Expr::Literal {
                        lit: n.1,
                        ty: Type::Int,
                    },
                    n.2,
                )
            }),
            barcexpr,
        )),
    )))(source)?;
    let res = match unop {
        Some(u) => {
            let res = match u {
                "-" => Expr::Neg {
                    expr: Box::new(t1),
                    ty: None,
                },
                "!" => Expr::Not {
                    expr: Box::new(t1),
                    ty: None,
                },
                _ => unreachable!("unexpected unop"),
            };
            res
        }
        None => t1.1,
    };
    let mut res = (start_idx, res, first_idx);
    for (op, lit) in exs {
        let curr_end = lit.2;
        let r = match op {
            "*" => Expr::Arith {
                op: ArithOp::Mul,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "/" => Expr::Arith {
                op: ArithOp::Div,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "%" => Expr::Arith {
                op: ArithOp::Mod,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            _ => unreachable!("unexpected operator"),
        };
        res = (res.0, r, curr_end);
    }
    Ok((source, res))
}

fn term<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let (source, t1) = context("expecting * or / or %", factor)(source)?;
    let (source, exs) = many0(tuple((
        delimited(span_space0, alt((spantag("+"), spantag("-"))), span_space0),
        factor,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match op {
            "+" => Expr::Arith {
                op: ArithOp::Add,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "-" => Expr::Arith {
                op: ArithOp::Sub,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            _ => unreachable!("unexpected operator"),
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

fn comparision<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let (source, t1) = term(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            span_space0,
            alt((
                spantag("<="),
                spantag(">="),
                spantag("<"),
                spantag(">"),
                spantag("=="),
                spantag("!="),
            )),
            span_space0,
        ),
        term,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match op {
            "<" => Expr::CmpOp {
                op: CmpOp::Lt,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            ">" => Expr::CmpOp {
                op: CmpOp::Gt,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "<=" => Expr::CmpOp {
                op: CmpOp::Le,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            ">=" => Expr::CmpOp {
                op: CmpOp::Ge,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "==" => Expr::CmpOp {
                op: CmpOp::Eq,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "!=" => Expr::CmpOp {
                op: CmpOp::Ne,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            _ => unreachable!("unexpected operator"),
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

pub fn aexp<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let (source, t1) = comparision(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            span_space0,
            alt((spantag("&&"), spantag("||"))),
            span_space0,
        ),
        comparision,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match op {
            "&&" => Expr::BoolOp {
                op: BoolOp::And,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            "||" => Expr::BoolOp {
                op: BoolOp::Or,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            _ => unreachable!("unexpected operator"),
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

fn chanread<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = spantag("<-")(source)?;
    let (source, id) = context("identifier", identifier)(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Expr::ChanRead { name: id, ty: None }, end_idx),
    ))
}

fn assignment<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, lhs) = alt((subscript, variable))(source)?;
    let (source, _) = delimited(span_space0, spantag("="), span_space0)(source)?;
    let (source, expr) = alt((chanread, aexp, make))(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (
            start_idx,
            Expr::Assign {
                left: Box::new(lhs),
                right: Box::new(expr),
            },
            end_idx,
        ),
    ))
}

fn make<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = spantag("make")(source)?;
    let (source, _) = spantag("(")(source)?;
    let (source, ty) = delimited(span_space0, alt((listty, chanty, mapty)), span_space0)(source)?;
    let (source, len) = opt(map(
        tuple((spantag(","), delimited(span_space0, aexp, span_space0))),
        |(_, l)| Box::new(l),
    ))(source)?;
    let (source, _) = spantag(")")(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Expr::Make { ty, expr: len }, end_idx)))
}

pub fn expression<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    alt((chanread, assignment, range, make, list, tuplee))(source)
}

fn list<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = spantag("[")(source)?;
    let (source, exprs) = many1(map(
        tuple((
            expression,
            delimited(span_space0, spantag(","), span_space0),
        )),
        |(e, _)| e,
    ))(source)?;
    let (source, _) = spantag("]")(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Expr::List { exprs, ty: None }, end_idx)))
}

fn range<'a>(source: &'a str) -> IResult<&'a str, Spanned<Expr>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, start) = aexp(source)?;
    let (source, rest) = opt(tuple((
        spantag(".."),
        opt(spantag("=")),
        delimited(span_space0, aexp, span_space0),
    )))(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    if let Some((_, equal, end)) = rest {
        Ok((
            source,
            (
                start_idx,
                Expr::Range {
                    start: Box::new(start),
                    end: Box::new(end),
                    inclusive: equal.is_some(),
                },
                end_idx,
            ),
        ))
    } else {
        Ok((source, start))
    }
}

fn declaration<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("let"), span_space0)(source)?;
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(span_space0, spantag("="), span_space0)(source)?;
    let (source, value) = expression(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Stmt::Decl(name, Box::new(value)), end_idx),
    ))
}

fn forin<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("for"), span_space0)(source)?;
    let (source, id) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(span_space0, spantag("in"), span_space0)(source)?;
    let (source, ex2) = expression(source)?;
    let (source, stmts) = block(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Stmt::For(id, Box::new(ex2), stmts), end_idx),
    ))
}

fn block<'a>(source: &'a str) -> IResult<&'a str, Vec<Spanned<Stmt>>, ErrorPos<'a>> {
    let (source, _) = delimited(faaaltu, spantag("{"), faaaltu)(source)?;
    let (source, stmts) = many0(delimited(faaaltu, statement, faaaltu))(source)?;
    let (source, _) = delimited(faaaltu, spantag("}"), faaaltu)(source)?;
    Ok((source, stmts))
}

fn ifelse<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("if"), span_space0)(source)?;
    let (source, ex1) = expression(source)?;
    let (source, thenst) = block(source)?;
    let (source, _) = opt(delimited(span_space0, spantag("else"), span_space0))(source)?;
    let (source, elseif) = opt(ifelse)(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    match elseif {
        Some(elseif) => Ok((
            source,
            (
                start_idx,
                Stmt::IfElse(Box::new(ex1), thenst, Some(vec![elseif])),
                end_idx,
            ),
        )),
        None => {
            let (source, elsest) = opt(block)(source)?;
            Ok((
                source,
                (
                    start_idx,
                    Stmt::IfElse(Box::new(ex1), thenst, elsest),
                    end_idx,
                ),
            ))
        }
    }
}

fn whileloop<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("while"), span_space0)(source)?;
    let (source, ex1) = delimited(span_space0, expression, span_space0)(source)?;
    let (source, stmts) = block(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Stmt::While(Box::new(ex1), stmts), end_idx),
    ))
}

fn breakst<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("break"), span_space0)(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Stmt::Break, end_idx)))
}

fn continuest<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("continue"), span_space0)(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Stmt::Continue, end_idx)))
}

fn returnst<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("return"), span_space0)(source)?;
    let (source, ex) = expression(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Stmt::Return(Box::new(ex)), end_idx)))
}

fn coroutine<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, _) = delimited(span_space0, spantag("sahl"), span_space0)(source)?;
    let (source, fncall) = call(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((source, (start_idx, Stmt::Coroutine(fncall), end_idx)))
}

fn chanwrite<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let start_idx = IDX.load(Ordering::Relaxed);
    let (source, id) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(span_space0, spantag("<-"), span_space0)(source)?;
    let (source, ex) = expression(source)?;
    let end_idx = IDX.load(Ordering::Relaxed);
    Ok((
        source,
        (start_idx, Stmt::ChanWrite(id, Box::new(ex)), end_idx),
    ))
}

fn statement<'a>(source: &'a str) -> IResult<&'a str, Spanned<Stmt>, ErrorPos<'a>> {
    let (source, stmt) = alt((
        context("chanwrite", chanwrite),
        context("coroutine", coroutine),
        context("forin loop", forin),
        context("ifelse", ifelse),
        context("while loop", whileloop),
        context("declaration", declaration),
        context("return", returnst),
        context("break", breakst),
        context("continue", continuest),
        map(expression, |ex| {
            (ex.0, Stmt::Expr(Box::new(ex.clone())), ex.2)
        }),
    ))(source)?;
    Ok((source, stmt))
}

fn parameter<'a>(source: &'a str) -> IResult<&'a str, Param, ErrorPos<'a>> {
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(span_space0, spantag(":"), span_space0)(source)?;
    let (source, ty) = alt((listty, typee))(source)?;
    Ok((source, Param { name, ty }))
}

fn function<'a>(source: &'a str) -> IResult<&'a str, Func, ErrorPos<'a>> {
    let (source, _) = delimited(span_space0, spantag("fun"), span_space0)(source)?;
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(span_space0, spantag("("), span_space0)(source)?;
    let (source, arg) = opt(parameter)(source)?;
    let mut args = vec![];
    let source = if arg.is_some() {
        let (source, args2) = many0(map(
            tuple((delimited(span_space0, spantag(","), span_space0), parameter)),
            |(_, p)| p,
        ))(source)?;
        let (source, _) = delimited(span_space0, spantag(")"), span_space0)(source)?;
        args = vec![arg.unwrap()].into_iter().chain(args2).collect();
        source
    } else {
        let (source, _) = delimited(span_space0, spantag(")"), span_space0)(source)?;
        source
    };
    let (source, retty) = opt(map(
        tuple((delimited(span_space0, spantag("->"), span_space0), typee)),
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

pub fn program<'a>(source: &'a str) -> IResult<&'a str, Program, ErrorPos<'a>> {
    let (source, funcs) = many1(delimited(faaaltu, function, faaaltu))(source)?;
    if source.len() > 0 {
        return Err(Err::Error(ErrorPos::from_error_kind(
            source,
            ErrorKind::Eof,
        )));
    }
    Ok((source, Program { funcs }))
}

use crate::syntax::*;
extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::{none_of, space0},
    combinator::{map, opt, cut, fail},
    error::{context, VerboseError},
    multi::{many0, many1},
    number::complete::float,
    sequence::{delimited, tuple},
    IResult,
};
use nom_supreme::final_parser::final_parser;
use std::str;
use nom_locate::{LocatedSpan, position};

type SpanStr<'a> = LocatedSpan<&'a str>;

fn whitespace<'a>(input: SpanStr<'a>) -> IResult<SpanStr<'a>, SpanStr<'a>, VerboseError<SpanStr<'a>>> {
    let (source, consumed) =
        take_while(|c: char| c.is_whitespace() || c == '\n' || c == '\r')(input)?;
    Ok((source, consumed))
}

fn comment<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, SpanStr<'a>, VerboseError<SpanStr<'a>>> {
    let (source, _) = whitespace(source)?;
    let (source, _) = tag("//")(source)?;
    let (source, s) = take_while(|c: char| c != '\n')(source)?;
    Ok((source, s))
}

fn faaaltu<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, SpanStr<'a>, VerboseError<SpanStr<'a>>> {
    let (source, _) = many0(comment)(source)?;
    let (source, consumeed) = whitespace(source)?;
    Ok((source, consumeed))
}


fn typee<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Type, VerboseError<SpanStr<'a>>> {
    let (source, ty) = alt::<_, _, _, _>((
        listty,
        tuplety,
        map(tag("int"), |_| Type::Int),
        map(tag("bool"), |_| Type::Bool),
        map(tag("string"), |_| Type::Str),
        map(tag("char"), |_| Type::Char),
        map(tag("double"), |_| Type::Double),
        chanty,
        mapty,
    ))(source)?;
    Ok((source, ty))
}

fn mapty<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Type, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("map")(source)?;
    let (source, _) = tag("<")(source)?;
    let (source, _) = space0(source)?;
    let (source, key) = typee(source)?;
    let (source, _) = tag(",")(source)?;
    let (source, _) = space0(source)?;
    let (source, val) = typee(source)?;
    let (source, _) = space0(source)?;
    let (source, _) = tag(">")(source)?;
    Ok((source, Type::Map(Box::new(key), Box::new(val))))
}

fn chanty<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Type, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("chan<")(source)?;
    let (source, _) = space0(source)?;
    let (source, ty) = typee(source)?;
    let (source, _) = space0(source)?;
    let (source, _) = tag(">")(source)?;
    Ok((source, Type::Chan(Box::new(ty))))
}

fn listty<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Type, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("[")(source)?;
    let (source, ty) = alt((listty, typee))(source)?;
    let (source, _) = tag("]")(source)?;
    Ok((source, Type::List(Box::new(ty))))
}

fn tuplety<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Type, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("(")(source)?;
    let (source, mut tys) = many0(map(
        tuple((space0, typee, space0, tag(","))),
        |(_, e, _, _)| e,
    ))(source)?;
    let (source, last) = opt(map(tuple((space0, typee, space0)), |(_, e, _)| e))(source)?;
    let (source, _) = tag(")")(source)?;
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
        "let" | "in" | "if" | "then" | "else" | "true" | "false" | "fun" | "return" | "make" | "cast" => {
            true
        }
        _ => false,
    }
}

fn identifier<'a>(input: SpanStr<'a>) -> IResult<SpanStr<'a>, String, VerboseError<SpanStr<'a>>> {
    let (input, id1) = take_while_m_n(1, 1, |c: char| c.is_alphabetic() || c == '_')(input)?;
    let (input, id2) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    let id = format!("{}{}", id1, id2);
    if isreserved(id.as_str()) {
        context("reserved keyword", fail)(input)
    } else {
        Ok((input, id))
    }
}

fn parse_escape<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, u8, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("\\")(source)?;
    let (source, esc) = alt((
        map(tag("n"), |_| '\n'),
        map(tag("r"), |_| '\r'),
        map(tag("t"), |_| '\t'),
        map(tag("\\"), |_| '\\'),
    ))(source)?;
    Ok((source, esc as u8))
}

fn string<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Lit>, VerboseError<SpanStr<'a>>> {
    let start = position(source)?;
    let start = start.0.location_offset();
    let (source, _) = tag("\"")(source)?;
    let (source, chars) = many0(alt((parse_escape, map(none_of("\""), |c| c as u8))))(source)?;
    let (source, _) = tag("\"")(source)?;
    let end = position(source)?;
    let end = end.1.location_offset();
    Ok((source, (start, Lit::Str(chars), end)))
}

fn charr<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Lit>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, c) = delimited(
        tag("'"),
        take_while_m_n(1, 2, |c: char| c != '\''),
        tag("'"),
    )(source)?;
    if c.as_bytes()[0] == '\\' as u8 {
        let (source, c) = parse_escape(c)?;
        let end_idx = position(source)?;
        let end_idx = end_idx.1.location_offset();
        Ok((source, (start_idx, Lit::Char(c), end_idx)))
    } else {
        Ok((
            source,
            (start_idx, Lit::Char(c.as_bytes()[0]), start_idx + 1),
        ))
    }
}

fn natural<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Lit>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, nat) = take_while_m_n(1, 19, |c: char| c.is_digit(10))(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Lit::Int(nat.parse::<i64>().unwrap()), end_idx),
    ))
}

fn floatt<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Lit>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, f) = float(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Lit::Double(f as f64), end_idx)))
}

fn boolean<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Lit>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, lit) = map(alt((tag("true"), tag("false"))), |s: SpanStr| {
        Lit::Bool(s.len() == 4 && s.ends_with("true"))
    })(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, lit, end_idx)))
}

fn tuplee<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = tag("(")(source)?;
    let (source, mut exprs) = many0(map(
        tuple((space0, expression, space0, tag(","))),
        |(_, e, _, _)| e,
    ))(source)?;
    let (source, last) = opt(map(
        tuple((space0, expression, space0)),
        |(_, e, _)| e,
    ))(source)?;
    let (source, _) = tag(")")(source)?;
    match last {
        Some(e) => {
            exprs.push(e);
        }
        None => {}
    };
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    let ex = Expr::Tuple { exprs, ty: None };
    Ok((source, (start_idx, ex, end_idx)))
}

fn subscript<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, subscrs) = many0(map(
        tuple((tag("["), expression, tag("]"))),
        |(_, expr, _)| expr,
    ))(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
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

fn call<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = tag("(")(source)?;
    let (source, arg) = opt(expression)(source)?;
    let (source, args2) = many0(map(
        tuple((
            tag(","),
            space0,
            context("argument for function call", expression),
            space0,
        )),
        |(_, _, e, _)| e,
    ))(source)?;
    let (source, _) = tag(")")(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
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

fn variable<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, name) = context("identifier", identifier)(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Expr::Variable { name, ty: None }, end_idx),
    ))
}

fn barcexpr<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, _) = tag("(")(source)?;
    let (source, expr) = delimited(space0, expression, space0)(source)?;
    let (source, _) = tag(")")(source)?;
    Ok((source, expr))
}

fn prim<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
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
        cast,
        call,
        subscript,
        variable,
        barcexpr,
    ))(source)?;
    Ok((source, t1))
}

fn factor<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, unop) = opt(alt((tag("-"), tag("!"))))(source)?;
    let (source, t1) = prim(source)?;
    let first_idx  = t1.0;
    let (source, exs) = many0(tuple((
        delimited(
            space0,
            alt((tag("*"), tag("/"), tag("%"))),
            space0,
        ),
        factor,
    )))(source)?;
    let res = match unop {
        Some(u) => {
            let res = match &u[..1] {
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
        let r = match &op[..1] {
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

fn term<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = context("expecting * or / or %", factor)(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, alt((tag("+"), tag("-"))), space0),
        term,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match &op[..1] {
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

pub fn shift<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = term(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            space0,
            alt((tag("<<"), tag(">>"))),
            space0,
        ),
        term,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match &op[..1] {
            "<<" => Expr::BitOp {
                op: BitOp::Shl,
                left: Box::new(res.clone()),
                right: Box::new(lit.clone()),
                ty: None,
            },
            ">>" => Expr::BitOp {
                op: BitOp::Shr,
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

fn comparision<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = shift(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            space0,
            alt((
                tag("<="),
                tag(">="),
                tag("<"),
                tag(">"),
                tag("=="),
                tag("!="),
            )),
            space0,
        ),
        shift,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match &op[..] {
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

fn bitand<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = comparision(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, tag("&"), space0),
        comparision,
    )))(source)?;
    let mut res = t1;
    for (_op, lit) in exs {
        let r = Expr::BitOp {
            op: BitOp::And,
            left: Box::new(res.clone()),
            right: Box::new(lit.clone()),
            ty: None,
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

fn bitxor<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = bitand(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, tag("^"), space0),
        bitand,
    )))(source)?;
    let mut res = t1;
    for (_op, lit) in exs {
        let r = Expr::BitOp {
            op: BitOp::Xor,
            left: Box::new(res.clone()),
            right: Box::new(lit.clone()),
            ty: None,
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

fn bitor<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = bitxor(source)?;
    let (source, exs) = many0(tuple((
        delimited(space0, tag("|"), space0),
        bitxor,
    )))(source)?;
    let mut res = t1;
    for (_op, lit) in exs {
        let r = Expr::BitOp {
            op: BitOp::Or,
            left: Box::new(res.clone()),
            right: Box::new(lit.clone()),
            ty: None,
        };
        res = (res.0, r, lit.2);
    }
    Ok((source, res))
}

pub fn aexp<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let (source, t1) = bitor(source)?;
    let (source, exs) = many0(tuple((
        delimited(
            space0,
            alt((tag("&&"), tag("||"))),
            space0,
        ),
        bitor,
    )))(source)?;
    let mut res = t1;
    for (op, lit) in exs {
        let r = match &op[..] {
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

fn chanread<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = tag("<-")(source)?;
    let (source, id) = context("identifier", identifier)(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Expr::ChanRead { name: id, ty: None }, end_idx),
    ))
}

fn assignment<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, lhs) = alt((subscript, variable))(source)?;
    let (source, _) = delimited(space0, tag("="), space0)(source)?;
    let (source, expr) = alt((chanread, aexp, make))(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
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

fn make<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = tag("make")(source)?;
    let (source, _) = tag("(")(source)?;
    let (source, ty) = delimited(space0, alt((listty, chanty, mapty)), space0)(source)?;
    let (source, len) = opt(map(
        tuple((tag(","), delimited(space0, aexp, space0))),
        |(_, l)| Box::new(l),
    ))(source)?;
    let (source, _) = tag(")")(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Expr::Make { ty, expr: len }, end_idx)))
}

fn cast<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = tag("cast")(source)?;
    let (source, _) = tag("(")(source)?;
    let (source, expr) = delimited(space0, expression, space0)(source)?;
    let (source, _) = tag(",")(source)?;
    let (source, ty) = delimited(space0, typee, space0)(source)?;
    let (source, _) = tag(")")(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Expr::Cast { expr: Box::new(expr), ty }, end_idx)))
}

pub fn expression<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    alt((chanread, assignment, range, make, cast, list, tuplee))(source)
}

fn list<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = tag("[")(source)?;
    let (source, exprs) = many1(map(
        tuple((
            expression,
            delimited(space0, tag(","), space0),
        )),
        |(e, _)| e,
    ))(source)?;
    let (source, _) = tag("]")(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Expr::List { exprs, ty: None }, end_idx)))
}

fn range<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Expr>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, start) = aexp(source)?;
    let (source, rest) = opt(tuple((
        tag(".."),
        opt(tag("=")),
        delimited(space0, aexp, space0),
    )))(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
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

fn declaration<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("let"), space0)(source)?;
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(space0, tag("="), space0)(source)?;
    let (source, value) = expression(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Stmt::Decl(name, Box::new(value)), end_idx),
    ))
}

fn forin<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("for"), space0)(source)?;
    let (source, id) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(space0, tag("in"), space0)(source)?;
    let (source, ex2) = expression(source)?;
    let (source, stmts) = block(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Stmt::For(id, Box::new(ex2), stmts), end_idx),
    ))
}

fn block<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Vec<Spanned<Stmt>>, VerboseError<SpanStr<'a>>> {
    let (source, _) = delimited(faaaltu, tag("{"), faaaltu)(source)?;
    let (source, stmts) = many0(delimited(faaaltu, statement, faaaltu))(source)?;
    let (source, _) = delimited(faaaltu, tag("}"), faaaltu)(source)?;
    Ok((source, stmts))
}

fn ifelse<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("if"), space0)(source)?;
    let (source, ex1) = expression(source)?;
    let (source, thenst) = block(source)?;
    let (source, _) = opt(delimited(space0, tag("else"), space0))(source)?;
    let (source, elseif) = opt(ifelse)(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
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

fn whileloop<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("while"), space0)(source)?;
    let (source, ex1) = delimited(space0, expression, space0)(source)?;
    let (source, stmts) = block(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Stmt::While(Box::new(ex1), stmts), end_idx),
    ))
}

fn breakst<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("break"), space0)(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Stmt::Break, end_idx)))
}

fn continuest<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("continue"), space0)(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Stmt::Continue, end_idx)))
}

fn returnst<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("return"), space0)(source)?;
    let (source, ex) = expression(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Stmt::Return(Box::new(ex)), end_idx)))
}

fn coroutine<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, _) = delimited(space0, tag("sahl"), space0)(source)?;
    let (source, fncall) = call(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((source, (start_idx, Stmt::Coroutine(fncall), end_idx)))
}

fn chanwrite<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
    let start_idx = position(source)?;
    let start_idx = start_idx.0.location_offset();
    let (source, id) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(space0, tag("<-"), space0)(source)?;
    let (source, ex) = expression(source)?;
    let end_idx = position(source)?;
    let end_idx = end_idx.1.location_offset();
    Ok((
        source,
        (start_idx, Stmt::ChanWrite(id, Box::new(ex)), end_idx),
    ))
}

fn statement<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Spanned<Stmt>, VerboseError<SpanStr<'a>>> {
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

fn parameter<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Param, VerboseError<SpanStr<'a>>> {
    let (source, name) = context("identifier", identifier)(source)?;
    let (source, _) = delimited(space0, tag(":"), space0)(source)?;
    let (source, ty) = alt((listty, typee))(source)?;
    Ok((source, Param { name, ty }))
}

fn function<'a>(source: SpanStr<'a>) -> IResult<SpanStr<'a>, Func, VerboseError<SpanStr<'a>>> {
    let (source, _) = delimited(space0, tag("fun"), space0)(source)?;
    let (source, name) = context("identifier", identifier)(source)?;
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
        let (source, _) = delimited(space0, tag(")"), space0)(source)?;
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

pub fn program<'a>(source: &'a str) -> Result<Program, VerboseError<SpanStr<'a>>> {
    let sourcee = SpanStr::new(source);
    final_parser(cut(many1::<_, _, VerboseError<SpanStr>, _>(delimited(faaaltu, function, faaaltu))))(sourcee)
        .map(|funcs| Program { funcs })
}


#[cfg(test)]
mod tests {
    use super::*;

    fn check_is_range(expr: Expr, start: Expr, end: Expr, inc: bool) {
        match expr {
            Expr::Range {
                start: s,
                end: e,
                inclusive,
            } => {
                assert_eq!((*s).1, start);
                assert_eq!((*e).1, end);
                assert_eq!(inc, inclusive);
            }
            _ => {
                println!("{:?}", expr);
                assert!(false);
            }
        }
    }

    #[test]
    fn test_range() {
        let res = range(SpanStr::new("1..=2"));
        match res {
            Ok((_, (_, ex, _))) => match ex {
                Expr::Range {
                    start,
                    end,
                    inclusive,
                } => {
                    assert_eq!(
                        (*start).1,
                        Expr::Literal {
                            lit: Lit::Int(1),
                            ty: Type::Int
                        }
                    );
                    assert_eq!(
                        (*end).1,
                        Expr::Literal {
                            lit: Lit::Int(2),
                            ty: Type::Int
                        }
                    );
                    assert_eq!(inclusive, true);
                }
                _ => {
                    println!("{:?}", ex);
                    assert!(false);
                }
            },
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }

    // for in loop
    #[test]
    fn test_for_in() {
        let res = forin(SpanStr::new("for i in 1..=10 { }"));
        let res2 = forin(SpanStr::new("for i in start..end { }"));
        match res {
            Ok((_, (_, ex, _))) => match ex {
                Stmt::For(v, range, end) => {
                    assert_eq!(v, "i");
                    check_is_range(
                        (*range).1,
                        Expr::Literal {
                            lit: Lit::Int(1),
                            ty: Type::Int,
                        },
                        Expr::Literal {
                            lit: Lit::Int(10),
                            ty: Type::Int,
                        },
                        true,
                    );
                    assert_eq!(end.len(), 0);
                }
                _ => {
                    println!("{:?}", ex);
                    assert!(false);
                }
            },
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        match res2 {
            Ok((_, (_, ex, _))) => match ex {
                Stmt::For(v, range, end) => {
                    assert_eq!(v, "i");
                    check_is_range(
                        (*range).1,
                        Expr::Variable {
                            name: "start".to_string(),
                            ty: None,
                        },
                        Expr::Variable {
                            name: "end".to_string(),
                            ty: None,
                        },
                        false,
                    );
                    assert_eq!(end.len(), 0);
                }
                _ => {
                    println!("{:?}", ex);
                    assert!(false);
                }
            },
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}

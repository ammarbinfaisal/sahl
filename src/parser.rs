use crate::syntax::*;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{combinator::Validate, extra::Err, input::SpannedInput, prelude::*, Parser};

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    Int(i64),
    Double(f64),
    Str(Vec<char>),
    Char(char),
    Ident(&'src str),
    Bool(bool),
    // Keywords
    Let,
    In,
    If,
    Then,
    While,
    For,
    Else,
    True,
    False,
    Fun,
    Return,
    Make,
    Cast,
    Sahl,
    Break,
    Continue,
    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Ampersand,
    Pipe,
    Assign,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LeftShift,
    RightShift,
    And,
    Or,
    Not,
    Dot,
    Comma,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftArrow,
    RightArrow,
    Range,
    ForwSlashForwSlash,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Int(i) => write!(f, "{}", i),
            Token::Double(d) => write!(f, "{}", d),
            Token::Str(s) => write!(f, "\"{}\"", s.iter().collect::<String>()),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Fun => write!(f, "fun"),
            Token::Return => write!(f, "return"),
            Token::Make => write!(f, "make"),
            Token::Cast => write!(f, "cast"),
            Token::Sahl => write!(f, "sahl"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Caret => write!(f, "^"),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Assign => write!(f, "="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::LessThanOrEqual => write!(f, "<="),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterThanOrEqual => write!(f, ">="),
            Token::LeftShift => write!(f, "<<"),
            Token::RightShift => write!(f, ">>"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Not => write!(f, "!"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftArrow => write!(f, "<-"),
            Token::RightArrow => write!(f, "->"),
            Token::Range => write!(f, ".."),
            Token::ForwSlashForwSlash => write!(f, "//"),
        }
    }
}

type Span = SimpleSpan<usize>;

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<(Token<'a>, Span)>, Err<Rich<'a, char>>> {
    let digits = text::int(10);

    let int = just::<_, _, _>('-')
        .or_not()
        .then(digits.clone())
        .map(|(sign, s)| {
            let mut res = String::new();
            if sign.is_some() {
                res.push('-');
            }
            res.push_str(&s);
            res
        })
        .from_str::<i64>()
        .unwrapped();

    let frac = just('.').then(text::int(10)).map(|(_, s)| s);

    let exp = just('e')
        .or(just('E'))
        .then(one_of("+-").or_not())
        .then(text::int(10));

    let double = just('-')
        .or_not()
        .then(digits)
        .then(frac)
        .then(exp.or_not())
        .map(|(((sign, int), frac), exp)| {
            let mut s = String::new();
            if sign.is_some() {
                s.push('-');
            }
            s.push_str(&int);
            s.push('.');
            s.push_str(&frac);
            if let Some(((_, sign), exp)) = exp {
                s.push('e');
                if sign.is_some() {
                    s.push('-');
                }
                s.push_str(&exp);
            }
            s.parse::<f64>().unwrap()
        });

    let hexint = just('-')
        .or_not()
        .then(just('0'))
        .then(one_of("xX"))
        .then(text::int(16))
        .map(|(((sign, _), _), int)| {
            let mut s = String::new();
            if sign.is_some() {
                s.push('-');
            }
            s.push_str(&int);
            s.parse::<i64>().unwrap()
        });

    let octal = just('-')
        .or_not()
        .then(just('0'))
        .then(text::int(8))
        .map(|((sign, _), int)| {
            let mut s = String::new();
            if sign.is_some() {
                s.push('-');
            }
            s.push_str(&int);
            s.parse::<i64>().unwrap()
        });

    let binary = just('-')
        .or_not()
        .then(just('0'))
        .then(one_of("bB"))
        .then(text::int(2))
        .map(|(((sign, _), _), int)| {
            let mut s = String::new();
            if sign.is_some() {
                s.push('-');
            }
            s.push_str(&int);
            s.parse::<i64>().unwrap()
        });

    let int = int.or(hexint).or(octal).or(binary);

    let escape: Boxed<'_, '_, _, char, _> = just('\\')
        .then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('b').to('\x08'),
            just('f').to('\x0C'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('u').ignore_then(text::digits(16).exactly(4).slice().validate(
                |digits, span, emitter| {
                    char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                        emitter.emit(Rich::custom(span, "invalid unicode character"));
                        '\u{FFFD}' // unicode replacement character
                    })
                },
            )),
        )))
        .map(|(_, c)| c)
        .boxed();

    let string: Boxed<'_, '_, _, Vec<char>, _> = none_of("\\\"")
        .or(escape)
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('"'), just('"'))
        .boxed();

    let char_escape: Boxed<'_, '_, _, char, _> = just('\\')
        .ignore_then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
        )))
        .boxed();

    let char: Boxed<'_, '_, _, char, _> = just('\'')
        .ignore_then(none_of("\\'").or(char_escape))
        .then_ignore(just('\''))
        .boxed();

    let bool = just::<&str, _, _>("true")
        .to(true)
        .or(just::<&str, _, _>("false").to(false))
        .map(Token::Bool);

    let ident = text::ascii::ident().map(|s| match s {
        "let" => Token::Let,
        "in" => Token::In,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "true" => Token::True,
        "false" => Token::False,
        "fun" => Token::Fun,
        "return" => Token::Return,
        "while" => Token::While,
        "make" => Token::Make,
        "cast" => Token::Cast,
        "for" => Token::For,
        "sahl" => Token::Sahl,
        "break" => Token::Break,
        "continue" => Token::Continue,
        _ => Token::Ident(s),
    });

    let commnts = just("//")
        .then(none_of("\n").repeated())
        .then(just("\n"))
        .padded()
        .repeated()
        .boxed();

    let token = commnts
        .clone()
        .ignore_then(
            double
                .map(Token::Double)
                .or(int.map(Token::Int))
                .or(char.map(Token::Char))
                .or(string.map(Token::Str))
                .or(bool)
                .or(ident)
                .or(just("<-").to(Token::LeftArrow))
                .or(just("..").to(Token::Range))
                .or(just("->").to(Token::RightArrow))
                .or(just("==").to(Token::Equal))
                .or(just("!=").to(Token::NotEqual))
                .or(just("<<").to(Token::LeftShift))
                .or(just("<=").to(Token::LessThanOrEqual))
                .or(just("<").to(Token::LessThan))
                .or(just(">=").to(Token::GreaterThanOrEqual))
                .or(just(">>").to(Token::RightShift))
                .or(just(">").to(Token::GreaterThan))
                .or(just("&&").to(Token::And))
                .or(just("||").to(Token::Or))
                .or(just('+').to(Token::Plus))
                .or(just('-').to(Token::Minus))
                .or(just('*').to(Token::Star))
                .or(just('/').to(Token::Slash))
                .or(just('%').to(Token::Percent))
                .or(just('^').to(Token::Caret))
                .or(just('&').to(Token::Ampersand))
                .or(just('|').to(Token::Pipe))
                .or(just('=').to(Token::Assign))
                .or(just("!").to(Token::Not))
                .or(just(".").to(Token::Dot))
                .or(just(",").to(Token::Comma))
                .or(just(":").to(Token::Colon))
                .or(just(";").to(Token::Semicolon))
                .or(just("(").to(Token::LeftParen))
                .or(just(")").to(Token::RightParen))
                .or(just("[").to(Token::LeftBracket))
                .or(just("]").to(Token::RightBracket))
                .or(just("{").to(Token::LeftBrace))
                .or(just("}").to(Token::RightBrace)),
        )
        .then_ignore(commnts)
        .padded()
        .map_with_span(|x, span| (x, span));

    token.repeated().collect()
}

type ParserInput<'src, 'tokens> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

fn typee<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'src, 'tokens>, Type, Err<Rich<'tokens, Token<'src>, Span>>> {
    recursive(
        |t: Recursive<
            dyn Parser<
                'tokens,
                ParserInput<'src, 'tokens>,
                Type,
                Err<Rich<'tokens, Token<'src>, Span>>,
            >,
        >| {
            let simplety = choice((
                just(Token::Ident("int")).to(Type::Int),
                just(Token::Ident("bool")).to(Type::Bool),
                just(Token::Ident("string")).to(Type::Str),
                just(Token::Ident("char")).to(Type::Char),
                just(Token::Ident("double")).to(Type::Double),
            ))
            .boxed();

            let listty = just(Token::LeftBracket)
                .ignored()
                .then(t.clone())
                .then_ignore(just(Token::RightBracket))
                .map(|(_, ty)| Type::List(Box::new(ty)))
                .boxed();

            let chanty = just(Token::Ident("chan"))
                .then(just(Token::LessThan))
                .ignored()
                .then(t.clone())
                .then_ignore(just(Token::GreaterThan))
                .map(|(_, ty)| Type::Chan(Box::new(ty)))
                .boxed();

            let mapty = just(Token::Ident("map"))
                .then(just(Token::LessThan))
                .ignored()
                .then(t.clone())
                .then_ignore(just(Token::Comma))
                .then(t.clone())
                .then_ignore(just(Token::GreaterThan))
                .map(|((_, key), val)| Type::Map(Box::new(key), Box::new(val)))
                .boxed();

            let tuplety = t
                .clone()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map(|tys| Type::Tuple(tys))
                .boxed();

            tuplety.or(mapty).or(chanty).or(listty).or(simplety)
        },
    )
}

fn ident<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'src, 'tokens>, String, Err<Rich<'tokens, Token<'src>, Span>>>
{
    select! {
        Token::Ident(s) => s.to_string(),
    }
}

fn exp<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'src, 'tokens>, Spanned<Expr>, Err<Rich<'tokens, Token<'src>, Span>>>
{
    let variable = select! {
        Token::Ident(v) => Expr::Variable { name: v.to_string(), ty: None }
    };

    let exp = recursive(
        |p_exp: Recursive<
            dyn Parser<
                'tokens,
                ParserInput<'src, 'tokens>,
                Spanned<Expr>,
                Err<Rich<'tokens, Token<'src>, Span>>,
            >,
        >| {
            let bracketed = p_exp
                .clone()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .boxed();

            let prim = recursive(
                |p: Recursive<
                    dyn Parser<
                        'tokens,
                        ParserInput<'src, 'tokens>,
                        Spanned<Expr>,
                        Err<Rich<'tokens, Token<'src>, Span>>,
                    >,
                >| {
                    // cast(value, type)
                    let cast = just(Token::Cast)
                        .ignored()
                        .then(just(Token::LeftParen))
                        .ignored()
                        .then(p.clone())
                        .then_ignore(just(Token::Comma))
                        .then(typee())
                        .then_ignore(just(Token::RightParen))
                        .map(|((_, e), ty)| Expr::Cast {
                            expr: Box::new(e),
                            ty,
                        })
                        .boxed();

                    let sqbrac_ex = just(Token::LeftBracket)
                        .ignored()
                        .then(p_exp.clone())
                        .then_ignore(just(Token::RightBracket))
                        .map(|(_, e)| e)
                        .boxed();

                    // use sqbrac_ex to avoid left recursion
                    let subscript = variable
                        .clone()
                        .then(sqbrac_ex.clone().repeated().collect::<Vec<_>>())
                        .map(|(name, subs)| {
                            let mut res = name;
                            if subs.is_empty() {
                                return res;
                            }
                            let start = subs[0].0;
                            let mut end = subs[0].2;
                            for sub in subs {
                                end = sub.2;
                                res = Expr::Subscr {
                                    expr: Box::new((start, res, end)),
                                    index: Box::new(sub),
                                    ty: None,
                                };
                            }
                            res
                        })
                        .boxed();

                    let call = subscript
                        .clone()
                        .then_ignore(just(Token::LeftParen))
                        .then(
                            p_exp
                                .clone()
                                .separated_by(just(Token::Comma))
                                .collect::<Vec<_>>()
                                .or_not(),
                        ) // TODO: replace p by p__expr
                        .then_ignore(just(Token::RightParen))
                        .map(|(name, args)| {
                            let args = args.unwrap_or_default();
                            Expr::Call {
                                name: Box::new(name),
                                args,
                                ty: None,
                            }
                        });

                    let bool = select! {
                        Token::Bool(b) => Expr::Literal { lit: Lit::Bool(b), ty: Type::Bool }
                    };

                    let char = select! {
                        Token::Char(c) => Expr::Literal { lit: Lit::Char(c as u8), ty: Type::Char }
                    };

                    let string = select! {
                        Token::Str(s) => Expr::Literal { lit: Lit::Str(s.into_iter().map(|c| {
                            c as u8
                        }).collect()), ty: Type::Str }
                    };

                    let double = select! {
                        Token::Double(f) => Expr::Literal { lit: Lit::Double(f), ty: Type::Double }
                    };

                    let int = select! {
                        Token::Int(i) => Expr::Literal { lit: Lit::Int(i), ty: Type::Int }
                    };

                    // primary expression: (expr) | ident | literal
                    let prim = cast
                        .or(call)
                        .or(bool)
                        .or(subscript)
                        .or(char.clone())
                        .or(string.clone())
                        .or(double)
                        .or(int)
                        .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end))
                        .boxed();

                    prim
                },
            );

            let primm = one_of(vec![Token::Minus, Token::Not])
                .or_not()
                .then(prim.clone().or(bracketed))
                .map(|(op, e)| {
                    let start = e.0 - 1;
                    let end = e.2;
                    match op {
                        Some(t) => {
                            let ex = match t {
                                Token::Minus => Expr::Neg {
                                    expr: Box::new(e),
                                    ty: None,
                                },
                                Token::Not => Expr::Not {
                                    expr: Box::new(e),
                                    ty: None,
                                },
                                _ => unreachable!(),
                            };
                            (start, ex, end)
                        }
                        None => e,
                    }
                })
                .boxed();

            let factor = primm
                .clone()
                .foldl(
                    one_of(vec![Token::Star, Token::Slash, Token::Percent])
                        .then(primm.clone())
                        .repeated(),
                    |l, r| {
                        let (op, r) = r;
                        let start = l.0;
                        let end = r.2;
                        let ex = match op {
                            Token::Star => Expr::Arith {
                                op: ArithOp::Mul,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            Token::Slash => Expr::Arith {
                                op: ArithOp::Div,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            Token::Percent => Expr::Arith {
                                op: ArithOp::Mod,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            _ => unreachable!(),
                        };
                        (start, ex, end)
                    },
                )
                .boxed();

            let term = factor
                .clone()
                .foldl(
                    one_of(vec![Token::Plus, Token::Minus])
                        .then(factor.clone())
                        .repeated(),
                    |l, r| {
                        let (op, r) = r;
                        let start = l.0;
                        let end = r.2;
                        let ex = match op {
                            Token::Plus => Expr::Arith {
                                op: ArithOp::Add,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            Token::Minus => Expr::Arith {
                                op: ArithOp::Sub,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            _ => unreachable!(),
                        };
                        (start, ex, end)
                    },
                )
                .boxed();

            let shift = term
                .clone()
                .foldl(
                    one_of(vec![Token::LeftShift, Token::RightShift])
                        .then(term.clone())
                        .repeated(),
                    |l, r| {
                        let (op, r) = r;
                        let start = l.0;
                        let end = r.2;
                        let ex = match op {
                            Token::LeftShift => Expr::BitOp {
                                op: BitOp::Shl,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            Token::RightShift => Expr::BitOp {
                                op: BitOp::Shr,
                                left: Box::new(l),
                                right: Box::new(r),
                                ty: None,
                            },
                            _ => unreachable!(),
                        };
                        (start, ex, end)
                    },
                )
                .boxed();

            let cmpop = just(Token::Equal)
                .or(just(Token::NotEqual))
                .or(just(Token::GreaterThanOrEqual))
                .or(just(Token::LessThanOrEqual))
                .or(just(Token::LessThan))
                .or(just(Token::GreaterThan));

            let comparision = shift
                .clone()
                .foldl(cmpop.then(shift.clone()).repeated(), |l, r| {
                    let (op, r) = r;
                    let start = l.0;
                    let end = r.2;
                    let ex = match op {
                        Token::Equal => Expr::CmpOp {
                            op: CmpOp::Eq,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        Token::NotEqual => Expr::CmpOp {
                            op: CmpOp::Ne,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        Token::LessThan => Expr::CmpOp {
                            op: CmpOp::Lt,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        Token::LessThanOrEqual => Expr::CmpOp {
                            op: CmpOp::Le,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        Token::GreaterThan => Expr::CmpOp {
                            op: CmpOp::Gt,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        Token::GreaterThanOrEqual => Expr::CmpOp {
                            op: CmpOp::Ge,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        _ => unreachable!(),
                    };
                    (start, ex, end)
                })
                .boxed();

            let bitand = recursive(
                |pbitand: Recursive<
                    dyn Parser<
                        'tokens,
                        ParserInput<'src, 'tokens>,
                        Spanned<Expr>,
                        Err<Rich<'tokens, Token<'src>, Span>>,
                    >,
                >| {
                    comparision
                        .clone()
                        .then(just(Token::Ampersand).then(pbitand).or_not())
                        .map(|(l, r)| {
                            if let Some((_, r)) = r {
                                let start = l.0;
                                let end = r.2;
                                (
                                    start,
                                    Expr::BitOp {
                                        op: BitOp::And,
                                        left: Box::new(l),
                                        right: Box::new(r),
                                        ty: None,
                                    },
                                    end,
                                )
                            } else {
                                l
                            }
                        })
                        .boxed()
                },
            );

            let bitor = bitand
                .clone()
                .foldl(just(Token::Pipe).then(bitand.clone()).repeated(), |l, r| {
                    let (op, r) = r;
                    let start = l.0;
                    let end = r.2;
                    let ex = match op {
                        Token::Pipe => Expr::BitOp {
                            op: BitOp::Or,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        _ => unreachable!(),
                    };
                    (start, ex, end)
                })
                .boxed();

            let bitxor = bitor
                .clone()
                .foldl(just(Token::Caret).then(bitor.clone()).repeated(), |l, r| {
                    let (op, r) = r;
                    let start = l.0;
                    let end = r.2;
                    let ex = match op {
                        Token::Caret => Expr::BitOp {
                            op: BitOp::Xor,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        _ => unreachable!(),
                    };
                    (start, ex, end)
                })
                .boxed();

            let logicand = bitxor
                .clone()
                .foldl(just(Token::And).then(bitxor.clone()).repeated(), |l, r| {
                    let (op, r) = r;
                    let start = l.0;
                    let end = r.2;
                    let ex = match op {
                        Token::And => Expr::BoolOp {
                            op: BoolOp::And,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        _ => unreachable!(),
                    };
                    (start, ex, end)
                })
                .boxed();

            let logicor = logicand
                .clone()
                .foldl(just(Token::Or).then(logicand.clone()).repeated(), |l, r| {
                    let (op, r) = r;
                    let start = l.0;
                    let end = r.2;
                    let ex = match op {
                        Token::Or => Expr::BoolOp {
                            op: BoolOp::Or,
                            left: Box::new(l),
                            right: Box::new(r),
                            ty: None,
                        },
                        _ => unreachable!(),
                    };
                    (start, ex, end)
                })
                .boxed();

            let aexp = logicor;

            // [expr, expr, expr]
            let list = just(Token::LeftBracket)
                .ignored()
                .then(p_exp.clone().separated_by(just(Token::Comma)).collect())
                .then_ignore(just(Token::RightBracket))
                .map(|(_, exprs)| Expr::List { exprs, ty: None })
                .boxed();

            let tuple = just(Token::LeftParen)
                .ignored()
                .then(p_exp.clone().separated_by(just(Token::Comma)).collect())
                .then_ignore(just(Token::Comma))
                .then_ignore(just(Token::RightParen))
                .map(|(_, exprs)| Expr::Tuple { exprs, ty: None })
                .boxed();

            let chanread = just(Token::LeftArrow)
                .ignore_then(ident())
                .map(|name| Expr::ChanRead {
                    name: name.to_string(),
                    ty: None,
                })
                .boxed();

            let range = prim
                .clone()
                .then(just(Token::Range))
                .then(just(Token::Assign).or_not())
                .then(prim.clone())
                .map(|res| {
                    let inclusive = match res.0 .1 {
                        Some(_) => true,
                        None => false,
                    };
                    let left = res.0 .0 .0;
                    let right = res.1;
                    Expr::Range {
                        start: Box::new(left),
                        end: Box::new(right),
                        inclusive,
                    }
                })
                .boxed();

            let make = just(Token::Make)
                .ignored()
                .then(just(Token::LeftParen))
                .ignored()
                .then(typee())
                .then_ignore(just(Token::Comma).or_not())
                .then(p_exp.clone().or_not())
                .then_ignore(just(Token::RightParen))
                .map(|((_, ty), size)| Expr::Make {
                    ty,
                    expr: size.map(|e| Box::new(e)),
                })
                .boxed();

            // ensure that prim is only a variable or a subscript
            let assignment = prim
                .clone()
                .then(just(Token::Assign))
                .then(p_exp.clone())
                .map(|((left, _), right)| Expr::Assign {
                    left: Box::new(left),
                    right: Box::new(right),
                })
                .boxed();

            make.or(tuple)
                .or(list)
                .or(range)
                .or(assignment)
                .or(aexp.map(|(_, e, _)| e))
                .or(chanread)
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end))
        },
    );

    exp
}

fn statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'src, 'tokens>, Spanned<Stmt>, Err<Rich<'tokens, Token<'src>, Span>>>
{
    recursive(
        |st: Recursive<
            dyn Parser<
                'tokens,
                ParserInput<'src, 'tokens>,
                Spanned<Stmt>,
                Err<Rich<'tokens, Token<'src>, Span>>,
            >,
        >| {
            let block = st
                .clone()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .boxed();

            // recursive(|if_| {
            //     just(Token::If)
            //         .ignore_then(expr.clone())
            //         .then(block.clone())
            //         .then(
            //             just(Token::Else)
            //                 .ignore_then(block.clone().or(if_))
            //                 .or_not(),
            //         )
            //         .map_with_span(|((cond, a), b), span: Span| {
            //             (
            //                 Expr::If(
            //                     Box::new(cond),
            //                     Box::new(a),
            //                     // If an `if` expression has no trailing `else` block, we magic up one that just produces null
            //                     Box::new(b.unwrap_or_else(|| (Expr::Value(Value::Null), span.clone()))),
            //                 ),
            //                 span,
            //             )
            //         })
            // });

            let ifstmt = recursive(|if_| {
                just(Token::If)
                    .ignored()
                    .then(exp())
                    .then(block.clone())
                    .then(
                        just(Token::Else)
                            .ignore_then(
                                block
                                    .clone()
                                    .map(|stmts| {
                                        let start = stmts[0].0;
                                        let end = stmts[stmts.len() - 1].2;
                                        (start, Stmt::Block(stmts), end)
                                    })
                                    .or(if_.clone()),
                            )
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(((_, cond), body), elsee)| {
                        if elsee.is_empty() {
                            Stmt::IfElse(Box::new(cond), body, None)
                        } else {
                            Stmt::IfElse(Box::new(cond), body, Some(elsee))
                        }
                    })
                    .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end))
                    .boxed()
            });

            // .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let whilestmt = just(Token::While)
                .ignore_then(exp())
                .then(block.clone())
                .map(|(cond, body)| Stmt::While(Box::new(cond), body))
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let forstmt = just(Token::For)
                .ignored()
                .then(ident())
                .then_ignore(just(Token::In))
                .then(exp())
                .then(block.clone())
                .map(|(((_, name), expr), body)| Stmt::For(name.to_string(), Box::new(expr), body))
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let returnstmt = just(Token::Return)
                .ignored()
                .then(exp())
                .map(|(_, expr)| Stmt::Return(Box::new(expr)))
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let letstmt = just(Token::Let)
                .ignored()
                .then(ident())
                .then_ignore(just(Token::Assign))
                .then(exp())
                .map(|res| {
                    let ((_, name), expr) = res;
                    Stmt::Decl(name.to_string(), Box::new(expr))
                })
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let coro = just(Token::Sahl)
                .ignored()
                .then(exp())
                .map(|(_, expr)| Stmt::Coroutine(expr))
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let chanwrite = ident()
                .then(just(Token::LeftArrow))
                .then(exp())
                .map(|((name, _), expr)| Stmt::ChanWrite(name.to_string(), Box::new(expr)))
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let continuest = just(Token::Continue)
                .map(|_| Stmt::Continue)
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let breakstmt = just(Token::Break)
                .map(|_| Stmt::Break)
                .map_with_span(|e, span: SimpleSpan<usize>| (span.start, e, span.end));

            let finall: Boxed<'_, '_, _, Spanned<Stmt>, _> = ifstmt
                .or(whilestmt)
                .or(forstmt)
                .or(returnstmt)
                .or(letstmt)
                .or(coro)
                .or(chanwrite)
                .or(continuest)
                .or(breakstmt)
                .or(block.clone().map(|stmts| {
                    let start = stmts[0].0;
                    let end = stmts[stmts.len() - 1].2;
                    (start, Stmt::Block(stmts), end)
                }))
                .or(exp().map(|(l, e, r)| (l, Stmt::Expr(Box::new((l, e, r))), r)))
                .boxed();

            finall
        },
    )
}

// fun func_name(a: int, b: string) -> b {}
fn parse_function<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'src, 'tokens>, Func, Err<Rich<'tokens, Token<'src>, Span>>> {
    let param = ident().then(just(Token::Colon)).then(typee()).boxed();

    let params = param
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .map(|params| {
            params
                .into_iter()
                .map(|((name, _), ty)| Param {
                    name: name.to_string(),
                    ty,
                })
                .collect()
        });

    let block = statement()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .boxed();

    just(Token::Fun)
        .then(ident())
        .then(
            just(Token::LeftParen)
                .ignore_then(params.or_not())
                .then_ignore(just(Token::RightParen)),
        )
        .then_ignore(just(Token::RightArrow).or_not())
        .then(typee().or_not())
        .then(block.clone())
        .map(|res| {
            let ((((_, name), params), retty), body) = res;
            Func {
                name: name.to_string(),
                args: params.unwrap_or_default(),
                retty: retty.unwrap_or(Type::Void),
                body,
            }
        })
}

pub fn program(s: &str) -> Option<Program> {
    let lex = lexer();
    let (tokens, errors) = lex.parse(s).into_output_errors();
    let parser = parse_function()
        .repeated()
        .collect()
        .then_ignore(end())
        .map(|funcs| Program { funcs });
    if let Some(tokens) = &tokens {
        let (res, errs) = parser
            .parse(tokens.as_slice().spanned((s.len()..s.len()).into()))
            .into_output_errors();

        if let Some(prog) = res {
            return Some(prog);
        } else {
            handle_err(&errs, s);
        }
    } else {
        handle_err(&errors, s);
    };
    None
}

pub fn handle_err<T: Clone + std::fmt::Debug + std::fmt::Display>(e: &Vec<Rich<T>>, source: &str) {
    let mut reports = Vec::new();
    e.into_iter().for_each(|e| {
        let r = Report::build(ReportKind::Error, (), e.span().start)
            .with_message("Parsing Failed")
            .with_label(
                Label::new(e.span().into_range())
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish();
        reports.push(r);
    });
    for r in reports {
        let _ = r.print(Source::from(&source));
    }
}

#[test]
fn test_expression() {
    let sources = vec![
        "((3 * 8) ^ 4)",
        "a = make([int], 1000 / (9 + 2))",
        "a[i] && b[i][j]",
        "true || false",
        "a = 3 - 8 | 4",
        "a[0][j] = a[i][k]",
    ];

    let lexer = lexer();

    let mut tokenss = Vec::new();

    for source in sources {
        let (tokens, err) = lexer.parse(source).into_output_errors();
        handle_err(&err, source);
        tokenss.push((tokens, source));
    }

    for (tokens, source) in tokenss {
        if let Some(tokens) = tokens {
            let parser = exp().then_ignore(end()).boxed();
            let res = parser.clone().parse(
                tokens
                    .as_slice()
                    .spanned((source.len()..source.len()).into()),
            );
            handle_err(&res.clone().into_output_errors().1, source);
            let res = res.into_result();
            println!("{:?}", res);
            assert!(res.is_ok());
        }
    }
}

#[test]
fn test_statements() {
    let sources = vec![
        "if true { 
            a[i] = b[i][j] 
        } else { 
            a[i] = b[i][j] 
        }",
        "while true { a[i] = b[i][j] }",
        "for i in 0..100 {
            sahl fcall(i)
        }",        
        "while x < 1600 {
            registers[0][0] = 2.0 * 1.6 * (x / 1600.0 - 0.5)
            registers[0][1] = 2.0 * 0.9 * (y / 900.0 - 0.5)
            i = 1
            while i < 4 {
                registers[i][0] = 0.0
                registers[i][1] = 0.0
                i = i + 1
            }
            i = 0
            while i < 256 && ((registers[1][0] * registers[1][0] + registers[1][1] * registers[1][1]) < 4) {
                interpret(registers, code)
                i = i + 1
            }
            line[x] = cast(i, char)
            x = x + 1
        }        "
    ];

    let lexer = lexer();

    let mut tokenss = Vec::new();

    for source in sources {
        let (tokens, err) = lexer.parse(source).into_output_errors();
        handle_err(&err, source);
        tokenss.push((tokens, source));
    }

    for (tokens, source) in tokenss {
        if let Some(tokens) = tokens {
            let parser = statement().then_ignore(end()).boxed();
            let res = parser.clone().parse(
                tokens
                    .as_slice()
                    .spanned((source.len()..source.len()).into()),
            );
            handle_err(&res.clone().into_output_errors().1, source);
            let res = res.into_result();
            println!("{:?}", res);
            assert!(res.is_ok());
        }
    }
}

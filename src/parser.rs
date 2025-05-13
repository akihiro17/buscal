use nom::{
    branch::alt,
    bytes::{
        complete::tag,
        streaming::{is_not, take_while_m_n},
    },
    character::{
        complete::{alpha1, alphanumeric1, char, line_ending, multispace0, multispace1},
        streaming::{newline, space0, tab},
    },
    combinator::{cut, map, map_opt, map_res, opt, recognize, value, verify},
    error::{FromExternalError, ParseError},
    multi::{self, fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    streaming::take,
    Finish, IResult, Offset, Parser, Slice,
};

use crate::{
    calc_offset,
    expression::{ExprEnum, Expression},
    statement::{Statement, Statements},
    typedecl::{ArrayTypeDecl, TypeDecl},
    Span,
};

// Statement
pub fn statements_finish(i: Span) -> Result<Statements, nom::error::Error<Span>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}

fn general_statement<'a>(last: bool) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Statement> {
    let terminator = move |i| -> IResult<Span, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |input| {
        alt((
            comment_statement,
            new_line_statement,
            var_def,
            terminated(var_assign, terminator),
            fn_def_statement,
            if_statement,
            for_statement,
            loop_statement,
            case_statement,
            terminated(return_statement, terminator),
            terminated(fail_statement, terminator),
            terminated(continue_statement, terminator),
            terminated(break_statement, terminator),
            terminated(
                expr_statement,
                delimited(multispace0, tag(";"), opt(line_ending)),
            ),
        ))(input)
    }
}

pub(crate) fn last_statement(input: Span) -> IResult<Span, Statement> {
    general_statement(true)(input)
}

pub(crate) fn statement(input: Span) -> IResult<Span, Statement> {
    general_statement(false)(input)
}

fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, mut stmts) = many0(statement)(i)?;
    let (i, last) = opt(last_statement)(i)?;
    let (i, _) = opt(multispace0)(i)?;
    if let Some(last) = last {
        stmts.push(last);
    }
    Ok((i, stmts))
}

fn var_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, (name, td, ex)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;
        let (i, _) = space_delimited(char(':'))(i)?;
        let (i, td) = type_decl(i)?;
        let (i, _) = space_delimited(char('='))(i)?;
        let (i, ex) = space_delimited(expr)(i)?;
        let (i, _) = delimited(multispace0, char(';'), opt(line_ending))(i)?;
        Ok((i, (name, td, ex)))
    })(i)?;

    Ok((
        i,
        Statement::VarDef {
            span: calc_offset(span, i),
            name,
            td,
            ex,
        },
    ))
}

fn if_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, (cond, t_case, f_case)) = cut(|i| {
        let (i, cond) = expr(i)?;
        // let (i, t_case) = delimited(open_brace, expr, close_brace)(i)?;
        let (i, t_case) = delimited(open_brace, statements, close_brace)(i)?;
        let (i, f_case) = opt(preceded(
            space_delimited(tag("else")),
            delimited(open_brace, statements, close_brace),
        ))(i)?;
        Ok((i, (cond, t_case, f_case)))
    })(i)?;

    Ok((i, Statement::If(cond, t_case, f_case)))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, (name, args, ret_type, stmts)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;
        let (i, _) = space_delimited(tag("("))(i)?;
        let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
        let (i, _) = space_delimited(tag(")"))(i)?;
        let (i, _) = space_delimited(tag("->"))(i)?;
        let (i, ret_type) = type_decl(i)?;
        let (i, stmts) = delimited(
            open_brace,
            statements,
            delimited(multispace0, char('}'), opt(line_ending)),
        )(i)?;

        Ok((i, (name, args, ret_type, stmts)))
    })(i)?;
    Ok((
        i,
        Statement::FnDef {
            name,
            args,
            ret_type,
            stmts,
        },
    ))
}

fn return_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, ex1) = expr(i)?;

    Ok((i, Statement::Return(ex1)))
}

fn fail_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("fail"))(i)?;
    let (i, ex1) = expr(i)?;

    Ok((i, Statement::Fail(ex1)))
}

fn continue_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue(i)))
}

fn break_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("break"))(i)?;
    Ok((i, Statement::Break(i)))
}

fn new_line_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = line_ending(i)?;
    Ok((i, Statement::NewLine(i)))
}

fn case_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("case"))(i)?;
    let (i, (ex, cases)) = cut(|i| {
        let (i, ex) = space_delimited(expr)(i)?;
        let (i, _) = space_delimited(tag("in"))(i)?;
        let (i, _) = space_delimited(open_brace)(i)?;
        let (i, cases) = many0(case)(i)?;

        Ok((i, (ex, cases)))
    })(i)?;

    let (i, _) = space_delimited(close_brace)(i)?;
    Ok((i, Statement::Case(ex, cases)))
}

fn case<'src>(i: Span<'src>) -> IResult<Span, (Expression<'src>, Vec<Statement<'src>>)> {
    let (i, pattern) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("=>"))(i)?;
    let (i, stmts) = cut(|i| {
        let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;

        Ok((i, stmts))
    })(i)?;
    let (i, _) = opt(space_delimited(char(',')))(i)?;

    Ok((i, (pattern, stmts)))
}

fn parse_comment<'a, E>(input: Span<'a>) -> IResult<Span<'a>, SpannedString<'a>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let start = input;

    let build_string = fold_many0(
        map(
            verify(is_not("\n\r"), |s: &Span| !s.fragment().is_empty()),
            |s: Span<'a>| s,
        ),
        String::new,
        |mut acc, fragment| {
            acc.push_str(*fragment);
            acc
        },
    );

    let (remaining, parsed) = delimited(tag("#"), build_string, space0)(input)?;

    Ok((
        remaining,
        SpannedString {
            span: start.slice(..input.offset(&remaining)),
            value: parsed,
        },
    ))
}

fn comment_statement(i: Span) -> IResult<Span, Statement> {
    let (i, str) = parse_comment(i)?;
    let (i, _) = line_ending(i)?;
    Ok((
        i,
        Statement::Comment {
            span: i,
            comment: str.value,
        },
    ))
}

fn for_statement(i: Span) -> IResult<Span, Statement> {
    let i0 = i;
    let (i, _) = space_delimited(tag("for"))(i)?;
    let (i, (name, from, to, stmts)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;
        let (i, _) = space_delimited(tag("in"))(i)?;
        let (i, from) = expr(i)?;
        let (i, _) = space_delimited(tag("to"))(i)?;
        let (i, to) = expr(i)?;
        let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
        Ok((i, (name, from, to, stmts)))
    })(i)?;

    Ok((
        i,
        Statement::For {
            span: calc_offset(i0, i),
            name,
            from,
            to,
            stmts,
        },
    ))
}

fn loop_statement(i: Span) -> IResult<Span, Statement> {
    let i0 = i;
    let (i, _) = space_delimited(tag("loop"))(i)?;
    let (i, stmts) = cut(|i| {
        let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
        Ok((i, stmts))
    })(i)?;

    Ok((
        i,
        Statement::Loop {
            span: calc_offset(i0, i),
            stmts,
        },
    ))
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;

    Ok((
        i,
        Statement::VarAssign {
            span: calc_offset(span, i),
            name,
            ex: Box::new(ex),
        },
    ))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_decl(i)?;

    Ok((i, (ident, td)))
}

fn type_decl(i: Span) -> IResult<Span, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;
    if *td == "array" {
        let (i, td2) = opt(delimited(tag("::<"), identifier, char('>')))(i)?;
        if let Some(array_type_decl) = td2 {
            Ok((
                i,
                match *array_type_decl {
                    "i64" => TypeDecl::Array(ArrayTypeDecl::I64),
                    "str" => TypeDecl::Array(ArrayTypeDecl::Str),
                    _ => {
                        return Err(nom::Err::Failure(nom::error::Error::new(
                            td,
                            nom::error::ErrorKind::Verify,
                        )));
                    }
                },
            ))
        } else {
            return Err(nom::Err::Failure(nom::error::Error::new(
                td,
                nom::error::ErrorKind::Verify,
            )));
        }
    } else {
        Ok((
            i,
            match *td {
                "i64" => TypeDecl::I64,
                "str" => TypeDecl::Str,
                "void" => TypeDecl::Void,
                "pattern" => TypeDecl::Pattern,
                "exit_status" => TypeDecl::ExitStatus,
                _ => {
                    return Err(nom::Err::Failure(nom::error::Error::new(
                        td,
                        nom::error::ErrorKind::Verify,
                    )));
                }
            },
        ))
    }
}

// Expression
fn parens(i: Span) -> IResult<Span, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn negation(i: Span) -> IResult<Span, Expression> {
    let (i, _) = space_delimited(tag("!"))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;

    Ok((i, Expression::new(ExprEnum::Negation(Box::new(expr)), i)))
}

fn expr(i: Span) -> IResult<Span, Expression> {
    let (r, init) = term(i)?;

    fold_many0(
        pair(
            space_delimited(alt((tag("+"), tag("-"), tag("&&"), tag("||")))),
            term,
        ),
        move || init.clone(),
        |acc, (op, val): (Span, Expression)| {
            let span = calc_offset(i, acc.span);
            match *op {
                "+" => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                "-" => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                "&&" => Expression::new(ExprEnum::And(Box::new(acc), Box::new(val)), span),
                "||" => Expression::new(ExprEnum::Or(Box::new(acc), Box::new(val)), span),
                _ => {
                    panic!("Additive expression should have '+' or '-' operator")
                }
            }
        },
    )(r)
}

fn term(i: Span) -> IResult<Span, Expression> {
    let (r, init) = factor(i)?;

    fold_many0(
        pair(
            space_delimited(alt((
                tag("*"),
                tag("/"),
                tag("=="),
                tag("!="),
                tag(">"),
                tag("<"),
            ))),
            factor,
        ),
        move || init.clone(),
        |acc, (op, val): (Span, Expression)| {
            let span = calc_offset(i, acc.span);
            match *op {
                "*" => Expression::new(ExprEnum::Mul(Box::new(acc), Box::new(val)), i),
                "/" => Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), i),
                "==" => Expression::new(ExprEnum::Eq(Box::new(acc), Box::new(val)), i),
                "!=" => Expression::new(ExprEnum::NotEq(Box::new(acc), Box::new(val)), i),
                ">" => Expression::new(ExprEnum::Gt(Box::new(acc), Box::new(val)), i),
                "<" => Expression::new(ExprEnum::Lt(Box::new(acc), Box::new(val)), i),
                _ => panic!("Multiplicative expression should have '*' or '/' operator"),
            }
        },
    )(r)
}

fn factor(i: Span) -> IResult<Span, Expression> {
    alt((
        str_literal,
        num_literal,
        pattern_literal,
        func_call,
        array,
        array_ident,
        ident,
        parens,
        negation,
    ))(i)
}

fn func_call(i: Span) -> IResult<Span, Expression> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, args) = delimited(
        multispace0,
        delimited(
            tag("("),
            many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
            tag(")"),
        ),
        multispace0,
    )(i)?;

    Ok((
        i,
        Expression {
            expr: ExprEnum::FnInvoke {
                span: ident,
                exprs: args,
                name: ident.to_string(),
            },
            span: i,
        },
    ))
}

fn array(i: Span) -> IResult<Span, Expression> {
    let (i, _) = space_delimited(char('['))(i)?;
    let (i, mut exprs) = many0(terminated(expr, space_delimited(char(','))))(i)?;
    let (i, mut last_expr) = many0(expr)(i)?;
    let (i, _) = space_delimited(char(']'))(i)?;

    exprs.append(&mut last_expr);
    Ok((
        i,
        Expression {
            expr: ExprEnum::Array(exprs),
            span: i,
        },
    ))
}

fn ident(i: Span) -> IResult<Span, Expression> {
    let (i, opt_ref) = space_delimited(opt(space_delimited(tag("&"))))(i)?;
    let (i, res) = space_delimited(identifier)(i)?;
    if opt_ref.is_none() {
        Ok((
            i,
            Expression {
                expr: ExprEnum::Ident(res),
                span: i,
            },
        ))
    } else {
        Ok((
            i,
            Expression {
                expr: ExprEnum::NameRef(res),
                span: i,
            },
        ))
    }
}

fn array_ident(i: Span) -> IResult<Span, Expression> {
    let (i, res) = space_delimited(identifier)(i)?;
    let (i, index) = delimited(char('['), expr, char(']'))(i)?;
    Ok((
        i,
        Expression {
            expr: ExprEnum::IndexedArray(res, Box::new(index)),
            span: i,
        },
    ))
}

fn identifier(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn num_literal(input: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::NumLiteral(v.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?),
            v,
        ),
    ))
}

// ref. https://github.com/rust-bakery/nom/blob/7.1.3/examples/string.rs

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a, E>(input: Span<'a>) -> IResult<Span<'a>, char, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parser. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map_res(parse_delimited_hex, |hex: Span<'a>| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: Span<'a>) -> IResult<Span<'a>, char, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    preceded(
        char('\\'),
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

fn parse_escaped_whitespace<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    preceded(char('\\'), multispace1)(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
    EscapedWS,
}

#[derive(Debug)]
struct SpannedString<'a> {
    span: Span<'a>, // start position
    value: String,
}

fn parse_fragment<'a, E>(input: Span<'a>, sep: &str) -> IResult<Span<'a>, StringFragment<'a>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    alt((
        map(|input| parse_literal(input, sep), StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

fn parse_string<'a, E>(input: Span<'a>) -> IResult<Span<'a>, SpannedString<'a>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let start = input;

    let build_string = fold_many0(
        |i| parse_fragment(i, "\"\\"),
        String::new,
        |mut acc, fragment| {
            match fragment {
                StringFragment::Literal(s) => acc.push_str(s.fragment()),
                StringFragment::EscapedChar(c) => acc.push(c),
                StringFragment::EscapedWS => {}
            }
            acc
        },
    );

    let (remaining, parsed) = delimited(char('"'), build_string, char('"'))(input)?;

    Ok((
        remaining,
        SpannedString {
            span: start.slice(..input.offset(&remaining)),
            value: parsed,
        },
    ))
}

fn str_literal(i: Span) -> IResult<Span, Expression> {
    let (i, str) = parse_string(i)?;
    Ok((i, Expression::new(ExprEnum::StrLiteral(str.value), i)))
}

fn parse_literal<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
    sep: &str,
) -> IResult<Span<'a>, Span<'a>, E> {
    verify(is_not(sep), |s: &Span| !s.fragment().is_empty())(input)
}

fn parse_pattern<'a, E>(input: Span<'a>) -> IResult<Span<'a>, SpannedString<'a>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let start = input;

    let build_string = fold_many0(
        |input| parse_fragment(input, "`"),
        String::new,
        |mut acc, fragment| {
            match fragment {
                StringFragment::Literal(s) => acc.push_str(s.fragment()),
                StringFragment::EscapedChar(c) => acc.push(c),
                StringFragment::EscapedWS => {}
            }
            acc
        },
    );

    let (remaining, parsed) = delimited(char('`'), build_string, char('`'))(input)?;

    Ok((
        remaining,
        SpannedString {
            span: start.slice(..input.offset(&remaining)),
            value: parsed,
        },
    ))
}

fn pattern_literal(i: Span) -> IResult<Span, Expression> {
    let (i, str) = parse_pattern(i)?;
    Ok((i, Expression::new(ExprEnum::PatternLiteral(str.value), i)))
}

fn space_delimited<'src, O, E>(
    f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
    E: ParseError<Span<'src>>,
{
    delimited(multispace0, f, multispace0)
}

pub fn open_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

pub fn close_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = delimited(multispace0, char('}'), opt(line_ending))(i)?;
    Ok((i, ()))
}

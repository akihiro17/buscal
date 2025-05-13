use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of, space0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser,
};

use crate::types;

// fn var_assign(i: &str) -> IResult<&str, types::Statement> {
//     let (i, name) = space_delimited(identifier)(i)?;
//     let (i, _) = space_delimited(char('='))(i)?;
//     let (i, expr) = space_delimited(expr)(i)?;
//     Ok((i, types::Statement::VarAssign(name, expr)))
// }

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

pub fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = self::space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

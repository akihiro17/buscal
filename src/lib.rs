pub mod evaluator;
pub mod expression;
pub mod function;
pub mod parser;
pub mod standard;
pub mod statement;
pub mod type_checker;
pub mod typedecl;

use nom::{InputTake, Offset};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
pub fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}

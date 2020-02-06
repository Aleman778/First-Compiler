
/***************************************************************************
 * Base parser implementation defines parsers for the base constructs
 * i.e. a file and its items an item can be for example a function.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, terminated, pair, tuple},
    branch::alt,
    multi::{many0, separated_list},
    error::context,
    Err,
};
use crate::sqrrlc_ast::*;
use crate::sqrrlc_parser::{
    comment::multispace_comment0,
    error::convert_error,
    ParseSpan,
    IResult,
    Parser,
};


named!(parse_int())

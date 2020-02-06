


/**
 * Parse a source file containing items such as functions.
 */
impl Parser for File {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
            }
}


/**
 * Parse an item can atm only be a function.
 */
impl Parser for Item {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "item",
            alt((
                map(FnItem::parse, |func| Item::Fn(func)),
                map(ForeignModItem::parse, |module| Item::ForeignMod(module)),
            ))
        )(input)
    }
}


/**
 * Parse a function.
 */
impl Parser for FnItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function",
            map(tuple((
                preceded(multispace0, tag("fn")),
                preceded(multispace1, ExprIdent::parse),
                FnDecl::parse,
                Block::parse,
            )),
                |(start, id, decl, block)| {
                    let block_clone = block.clone();
                    FnItem {
                        ident: id,
                        decl: decl,
                        block: block,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            block_clone.span.end, input.extra
                        ),
                    }
                }
            )
        )(input)
    }
}


impl Parser for ForeignFnItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "foreign function",
            map(tuple((
                preceded(multispace0, tag("fn")),
                preceded(multispace1, ExprIdent::parse),
                FnDecl::parse,
                preceded(multispace0, tag(";")),
            )),
                |(start, id, decl, semi)| {
                    ForeignFnItem {
                        ident: id,
                        decl: decl,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            LineColumn::new(semi.line, semi.get_column()),
                            input.extra),
                    }
                }
            )
        )(input)
    }
}


impl Parser for ForeignModItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "extern",
            map(tuple((
                preceded(multispace0, tag("extern")),
                opt(preceded(multispace0, LitStr::parse)),
                preceded(multispace0, tag("{")),
                many0(preceded(multispace_comment0, ForeignFnItem::parse)),
                preceded(multispace0, tag("}")),
            )),
                |(start, abi, _, items, end)| {
                    ForeignModItem {
                        abi,
                        items,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            LineColumn::new(end.line, end.get_column()),
                            input.extra),
                    }
                }
            )
        )(input)
    }
}


/**
 * Parse a function delcaration, this includes
 * the input arguments and optionally output type.
 */
impl Parser for FnDecl {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function declaration",
            map(tuple((
                preceded(multispace0, tag("(")),
                separated_list(
                    preceded(multispace0, tag(",")),
                    Argument::parse
                ),
                preceded(multispace0, tag(")")),
                opt(pair(
                    preceded(multispace0, tag("->")),
                    Ty::parse
                )),
            )),
                |(start, args, end, ret_ty)| {
                    let mut output;
                    let end_span;
                    match ret_ty {
                        Some(ty) => {
                            end_span = ty.1.span.end;
                            output = ty.1;
                        },
                        None => {
                            output = Ty::new();
                            end_span = LineColumn::new(end.line, end.get_column() + 1);
                            output.span = Span::from_bounds(end_span, end_span, input.extra);
                        },  
                    };
                    FnDecl {
                        inputs: args,
                        output: output,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            end_span, input.extra
                        ),
                    }
                }
            )
        )(input)
    }
}


/**
 * Parse a function argument.
 */
impl Parser for Argument {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "argument",
            map(tuple((
                opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
                ExprIdent::parse,
                preceded(multispace0, tag(":")),
                Ty::parse,
            )),
                |(mut_token, id, _, ty)| {
                    let start_lc = match mut_token {
                        Some(mutable) => LineColumn::new(mutable.line, mutable.get_column()),
                        None => id.span.start,
                    };
                    let end_lc = ty.span.end;
                    Argument {
                        mutable: mut_token.is_some(),
                        ident: id,
                        ty: ty,
                        span: Span::from_bounds(start_lc, end_lc, input.extra),
                    }
                }
            )
        )(input)
    }
}

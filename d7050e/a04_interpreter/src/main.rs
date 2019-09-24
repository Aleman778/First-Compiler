mod env;
mod interpreter;

// use a02_parser::{parse};

use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a str>;

use interpreter::eval_expr;
use crate::env::Env;


// use std::fs;

fn main() {
    // let input = "true == 5";
    // let input = "(10  +  15) - ((5 + 3) * 2)";
    // let input = "(3 > 4) || (100 < 343)";
    let input = "let x: i32 = 5;";
    let input2 = "let y: i32 = 5 + x;";
    let expr = a02_parser::parse_expr(Span::new(input));
    let expr2 = a02_parser::parse_expr(Span::new(input2));
    
    // println!("{:#?}", (expr.unwrap().1).1);
    let env = &mut Env::new();
    println!("{:#?}", eval_expr(expr.unwrap().1, env));
    println!("{:#?}", eval_expr(expr2.unwrap().1, env));
    println!("{:#?}", env);
    
    // let input = fs::read_to_string("test.sq")
        // .expect("Error reading the file");
    // let result = parse(input.as_str());
    
    // match result {
        // Ok(n) => println!("Ok: \nInput: {}\nResulting Tree:\n    {:#?}", input, n),
        // Err(e) => println!("Error: {:#?}", e),
    // }
}

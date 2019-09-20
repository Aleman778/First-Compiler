mod interpreter;

// use a02_parser::{parse};

use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a str>;

use interpreter::eval_expr;

// use std::fs;

fn main() {
    // let input = "true == 5";
    // let input = "(10  +  15) - ((5 + 3) * 2)";
    let input = "(3 > 4) || 100";
    let expr = a02_parser::parse_expr(Span::new(input));
    
    // println!("{:#?}", (expr.unwrap().1).1);
    println!("{:?}", eval_expr(expr.unwrap().1));
    
    // let input = fs::read_to_string("test.sq")
        // .expect("Error reading the file");
    // let result = parse(input.as_str());
    
    // match result {
        // Ok(n) => println!("Ok: \nInput: {}\nResulting Tree:\n    {:#?}", input, n),
        // Err(e) => println!("Error: {:#?}", e),
    // }
}

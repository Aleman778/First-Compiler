mod scope;
mod env;
mod interpreter;

// use std::fs;

fn main() {
    // let input = "!false";
    // let input = "true == 5";
    // let input = "(10  +  15) - ((5 + 3) * 2)";
    // let input = "(3 > 4) || (100 < 343)";
    // let input = "let x: i32 = 5;";
    let input = "fn main() {let a: i32 = add(40, 7);} fn add(a: i32, b: i32) -> i32 {return a + b;}";
    // let input = "fn main() { recursion(1000); } fn recursion() {}";
    // let ast = a02_parser::parse(input).unwrap();
    
    let ast = a02_parser::parse(input).unwrap();
    interpreter::eval(ast);
    
    // let input = fs::read_to_string("test.sq")
        // .expect("Error reading the file");
    // let result = parse(input.as_str());
    
    // match result {
        // Ok(n) => println!("Ok: \nInput: {}\nResulting Tree:\n    {:#?}", input, n),
        // Err(e) => println!("Error: {:#?}", e),
    // }
}

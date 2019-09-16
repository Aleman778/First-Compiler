/**
 * Required for reading files.
 */
use std::fs;


/**
 * Main method, program starts here.
 */
fn main() {
    // let input = "let hello30: i32 = 1 + 2   ;";
    // let input = "min(a + 25, 40 - -b);";
    // let input = "{ let mut a:i32 = 5; let b:i32 = 3; a = a + 5; min(a + b, a - b * a + a * b); }";
    // let input = "if a > 5 { a = a + 2; } else { a = a - 2; }";
    // let input = "while i < 10 { foo(); i = i + 1; }";
    // let input = "fn min(a: i32, b: i32) -> i32 { if a < b { return a; } else { return b; } }
    // fn max(a: i32, b: i32) -> i32 { if a > b { return a; } else { return b; } }";
    
    let input = fs::read_to_string("test.sq")
        .expect("Error reading the file");
    let result = a02_parser::parse(input.as_str());
    
    match result {
        Ok(n) => println!("Ok: \nInput: {}\nResulting Tree:\n    {:#?}", input, n),
        Err(e) => println!("Error: {:#?}", e),
    }
}

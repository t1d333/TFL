pub mod grammar;
pub mod parser;
pub mod stack;
use std::io::stdin;

fn main() {
    let mut grammar_s = "".to_string();
    for line in stdin().lines() {
        grammar_s = format!("{}\n{}", grammar_s, line.unwrap());
    }
    let g = grammar::Grammar::new(grammar_s);
    let parser = parser::Parser::new(g);

    if let Err(msg) = parser.parse("id") {
        println!("Parsing error: {}", msg);
    } else {
        println!("accepted");
    }
}

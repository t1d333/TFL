pub mod grammar;
pub mod parser;
use std::io::stdin;

fn main() {
    let mut grammar_s = "".to_string();
    for line in stdin().lines() {
        grammar_s = format!("{}\n{}", grammar_s, line.unwrap());
    }
    grammar_s.trim();
    let g = grammar::Grammar::new(grammar_s);
    let parser = parser::Parser::new(g);

    parser.parse("");
    // println!("{:?}", g);
}

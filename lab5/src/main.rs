pub mod grammar;
pub mod parser;
pub mod stack;
use clap::{ArgAction, Parser};
use std::io::stdin;

#[derive(Parser)]
pub struct Cli {
    #[clap(long, short, action=ArgAction::SetFalse)]
    table_printing_flag: bool,
    #[clap(long, short, action=ArgAction::SetFalse)]
    automata_printing_flag: bool,
    #[clap(long, short)]
    word: String,
}

fn main() {
    let cli = Cli::parse();
    let mut grammar_s = "".to_string();
    for line in stdin().lines() {
        grammar_s = format!("{}\n{}", grammar_s, line.unwrap());
    }
    let g = grammar::Grammar::new(grammar_s);
    let parser = parser::Parser::new(g);

    if cli.automata_printing_flag {
        let automata = parser.gen_parsing_automata();
        println!("PARSING AUTOMATA:");
        println!("{}\n", automata);
    }

    if cli.table_printing_flag {
        parser.print_table();
    }

    if let Err(msg) = parser.parse(&cli.word) {
        println!("PARSING RESULT:");
        println!("Parsing error: {}", msg);
    } else {
        println!("PARSING RESULT:");
        println!("accepted");
    }
}

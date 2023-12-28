use crate::grammar::Grammar;

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let new_grammar = format!(
            "{}' -> {}\n{}",
            grammar.start, grammar.start, grammar.grammar_str
        );

        Self {
            grammar: Grammar::new(new_grammar),
        }
    }

    fn find_first_and_follow(&self) {}
}

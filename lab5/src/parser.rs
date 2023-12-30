use std::collections::{HashMap, HashSet};

use crate::grammar::Grammar;

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
    max_len: i32,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let tmp_grammar = format!(
            "{}' -> {}\n{}",
            grammar.start, grammar.start, grammar.grammar_str
        );

        Self {
            grammar: Grammar::new(tmp_grammar),
            max_len: 0,
        }
    }

    fn union(&self, lhs: &HashSet<String>, rhs: &mut HashSet<String>) -> bool {
        let old_len = rhs.len();
        rhs.extend(lhs.clone());

        old_len != rhs.len()
    }

    fn find_first_and_follow(
        &self,
    ) -> (
        HashMap<String, HashSet<String>>,
        HashMap<String, HashSet<String>>,
    ) {
        let mut first: HashMap<String, HashSet<String>> = HashMap::new();
        let mut follow: HashMap<String, HashSet<String>> = HashMap::new();

        for c in &self.grammar.symbols {
            let mut set = HashSet::new();
            set.insert(c.clone());
            first.insert(c.clone(), set);
        }

        for c in &self.grammar.non_terminals {
            follow.insert(c.clone(), HashSet::new());
        }

        let start_symbol_set = follow.get_mut(&self.grammar.start).unwrap();
        start_symbol_set.insert("$".to_string());

        loop {
            let mut updated = false;

            for (head, bodies) in self.grammar.grammar.iter() {
                for body in bodies {
                    for symbol in body {
                        updated = updated
                            || self.union(
                                &first.get(&symbol.to_string()).unwrap().clone(),
                                first.get_mut(head).unwrap(),
                            );
                    }

                    let mut aux = follow[head].clone();
                    for symbol in body.iter().rev() {
                        if follow.contains_key(symbol) {
                            updated = updated || self.union(&aux, follow.get_mut(symbol).unwrap());
                        } else {
                            aux = first[symbol].clone();
                        }
                    }
                }
            }

            if !updated {
                return (first, follow);
            }
        }
    }

    fn find_follow(&self) {}
    fn closure(&self) {}

    fn goto(&self) {}

    fn construct_table(&self) {}

    fn generate_items(&self) {}

    pub fn parse(&self, input: &str) -> bool {
        let f = self.find_first_and_follow();
        println!("{:#?}", f.1);
        false
    }
}

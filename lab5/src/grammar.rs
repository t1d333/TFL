use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Grammar {
    pub grammar: HashMap<String, HashSet<String>>,
    pub start: String,
    pub terminals: HashSet<String>,
    pub non_terminals: HashSet<String>,
    pub grammar_str: String,
}

impl Grammar {
    pub fn new(grammar_str: String) -> Self {
        let new_grammar = grammar_str.trim();
        let mut grammar = HashMap::new();
        let mut terminals = HashSet::new();
        let mut non_terminals = HashSet::new();
        let mut start = "".to_string();

        for p in new_grammar.split('\n') {
            let tmp = p.split(" -> ").collect::<Vec<&str>>();
            assert_eq!(tmp.len(), 2);
            let head = tmp[0];
            let body = tmp[1];

            assert!(head.chars().all(|c| c.is_uppercase()));

            if start.len() == 0 {
                start = head.to_string();
            }

            non_terminals.insert(head.to_string());
            if let None = grammar.get(head) {
                grammar.insert(head.to_string(), HashSet::new());
            }

            let bodies = body.split('|').map(|s| s.trim());
            for b in bodies {
                let splited = b.split(' ').collect::<Vec<&str>>();

                let tmp = grammar.get_mut(head).unwrap();
                tmp.insert(b.to_string());

                for symbol in splited {
                    if symbol.chars().all(|c| c.is_uppercase()) {
                        non_terminals.insert(symbol.to_string());
                    } else {
                        terminals.insert(symbol.to_string());
                    }
                }
            }
        }

        Self {
            grammar,
            start,
            terminals,
            non_terminals,
            grammar_str,
        }
    }
}

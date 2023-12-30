use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Grammar {
    pub grammar: HashMap<String, HashSet<Vec<String>>>,
    pub start: String,
    pub terminals: HashSet<String>,
    pub non_terminals: HashSet<String>,
    pub grammar_str: String,
    pub symbols: HashSet<String>,
}

impl Grammar {
    pub fn new(grammar_str: String) -> Self {
        let grammar_str = grammar_str.trim().to_string();
        let mut grammar: HashMap<String, HashSet<Vec<String>>> = HashMap::new();
        let mut terminals = HashSet::new();
        let mut non_terminals = HashSet::new();
        let mut symbols: HashSet<String>;
        let mut start = "".to_string();

        for p in grammar_str.split('\n') {
            let tmp = p.split(" -> ").collect::<Vec<&str>>();
            assert_eq!(tmp.len(), 2);
            let head = tmp[0];
            let body = tmp[1];

            assert!(head
                .chars()
                .all(|c| c.is_ascii_uppercase() || !c.is_alphabetic()));

            if start.len() == 0 {
                start = head.to_string();
            }

            non_terminals.insert(head.to_string());
            if let None = grammar.get(head) {
                grammar.insert(head.to_string(), HashSet::new());
            }

            let bodies = body.split('|').map(|s| s.trim());
            for b in bodies {
                let splited: Vec<String> = b.split_whitespace().map(str::to_string).collect();

                let bodies_set = grammar.get_mut(head).unwrap();
                bodies_set.insert(splited.clone());

                for symbol in splited {
                    if symbol.chars().all(|c| c.is_uppercase()) {
                        non_terminals.insert(symbol.to_string());
                    } else {
                        terminals.insert(symbol.to_string());
                    }
                }
            }
        }

        symbols = terminals.clone();
        symbols.extend(non_terminals.clone());

        Self {
            grammar,
            start,
            terminals,
            non_terminals,
            grammar_str,
            symbols,
        }
    }
}

use std::{
    collections::{HashMap, HashSet},
    result, vec,
};

use crate::grammar::Grammar;

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
    max_len: usize,
    first: HashMap<String, HashSet<String>>,
    follow: HashMap<String, HashSet<String>>,
    action: Vec<String>,
    goto: Vec<String>,
    table_symbols: Vec<String>,
    items: Vec<HashMap<String, HashSet<Vec<String>>>>,
    indexed_grammar: Vec<(String, Vec<String>)>,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let tmp_grammar = format!(
            "{}' -> {}\n{}",
            grammar.start, grammar.start, grammar.grammar_str
        );

        let mut indexed_grammar = vec![];

        let grammar = Grammar::new(tmp_grammar);
        for (head, bodies) in &grammar.grammar {
            for body in bodies {
                indexed_grammar.push((head.clone(), body.clone()));
            }
        }

        let mut max_len = 0;
        for bodies in grammar.grammar.values() {
            for body in bodies {
                if body.len() > max_len {
                    max_len = body.len()
                }
            }
        }

        let mut res = Self {
            first: HashMap::new(),
            follow: HashMap::new(),
            action: grammar.terminals.clone().into_iter().collect(),
            goto: grammar.non_terminals.clone().into_iter().collect(),
            max_len,
            grammar,
            table_symbols: vec![],
            items: vec![],
            indexed_grammar: vec![],
        };

        res.goto.retain(|symbol| symbol.ne(&res.grammar.start));

        res.table_symbols.extend(res.action.clone());
        res.table_symbols.extend(res.goto.clone());

        res.find_first_and_follow();

        res.generate_items();

        return res;
    }

    fn union(&self, lhs: &HashSet<String>, rhs: &mut HashSet<String>) -> bool {
        let old_len = rhs.len();
        rhs.extend(lhs.clone());

        old_len != rhs.len()
    }

    fn find_first_and_follow(&mut self) {
        let mut first: HashMap<String, HashSet<String>> = HashMap::new();
        let mut follow: HashMap<String, HashSet<String>> = HashMap::new();

        for c in &self.grammar.symbols {
            let mut set = HashSet::new();
            if self.grammar.terminals.contains(c) {
                set.insert(c.clone());
            }

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
                    let symbol = &body[0];
                    updated = updated
                        || self.union(
                            &first.get(&symbol.to_string()).unwrap().clone(),
                            first.get_mut(head).unwrap(),
                        );

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
                self.first = first;
                self.follow = follow;
                return;
            }
        }
    }

    fn closure(
        &self,
        p: HashMap<String, HashSet<Vec<String>>>,
    ) -> HashMap<String, HashSet<Vec<String>>> {
        let mut result = p.clone();
        loop {
            let item_len = result.len();

            for (_, bodies) in result.clone() {
                for body in bodies.clone() {
                    if body.contains(&".".to_string()) && body.last().unwrap().ne(".") {
                        let after_dot_idx = body.iter().position(|s| s.eq(".")).unwrap() + 1;
                        let symbol_after_dot = &body[after_dot_idx];

                        if self.grammar.non_terminals.contains(symbol_after_dot) {
                            if !result.contains_key(symbol_after_dot) {
                                result.insert(symbol_after_dot.to_string(), HashSet::new());
                            }
                            for b in &self.grammar.grammar[symbol_after_dot] {
                                let mut tmp = vec![".".to_string()];
                                tmp.extend(b.clone());
                                result.get_mut(symbol_after_dot).unwrap().insert(tmp);
                            }
                        }
                    }
                }
            }

            if item_len == result.len() {
                return result;
            }
        }
    }

    fn goto(
        &self,
        item: &HashMap<String, HashSet<Vec<String>>>,
        symbol: &str,
    ) -> HashMap<String, HashSet<Vec<String>>> {
        let mut result = HashMap::new();

        for (head, bodies) in item {
            for body in bodies {
                if body.contains(&".".to_string()) && body.last().unwrap().ne(".") {
                    let pos = body.iter().position(|s| s.eq(".")).unwrap();
                    if body[pos + 1].eq(symbol) {
                        let mut shifted_dot = body.clone();
                        shifted_dot.remove(pos);
                        shifted_dot.insert(pos + 1, ".".to_string());

                        let mut tmp_item = HashMap::new();
                        let mut set = HashSet::new();
                        set.insert(shifted_dot);
                        tmp_item.insert(head.clone(), set);

                        for (closure_head, closure_bodies) in self.closure(tmp_item) {
                            if !result.contains_key(&closure_head) {
                                result.insert(closure_head.clone(), HashSet::new());
                            }

                            let set = result.get_mut(&closure_head).unwrap();
                            for closure_body in closure_bodies {
                                set.insert(closure_body);
                            }
                        }
                    }
                }
            }
        }

        return result;
    }

    fn generate_items(&mut self) {
        let mut tmp = HashMap::new();
        let mut start = self.grammar.start.clone();
        start.pop();
        let mut set = HashSet::new();
        set.insert(vec![".".to_string(), start]);

        tmp.insert(self.grammar.start.clone(), set);

        tmp.get_mut(&self.grammar.start);
        let mut result = vec![self.closure(tmp)];

        loop {
            let len = result.len();
            for item in result.clone() {
                for symbol in &self.grammar.symbols {
                    let goto = self.goto(&item, symbol);

                    if goto.len() > 0 && !result.contains(&goto) {
                        result.push(goto);
                    }
                }
            }

            if result.len() == len {
                self.items = result;
                break;
            }
        }
    }

    fn construct_table(&self) {
        let mut table = HashMap::new();

        for i in 0..self.items.len() {
            for symbol in &self.table_symbols {
                table.insert((i, symbol.clone()), "".to_string());
            }
        }

        for (i, item) in self.items.iter().enumerate() {
            for (head, bodies) in item {
                for body in bodies {
                    if body.contains(&".".to_string()) && body.last().unwrap().ne(".") {
                        let pos = body.iter().position(|c| c.eq(".")).unwrap() + 1;
                        let symbol = &body[pos];
                        let action = format!(
                            "s{}",
                            self.items
                                .iter()
                                .position(|j| j.eq(&self.goto(item, symbol)))
                                .unwrap()
                        );

                        let mut cell = table[&(i, symbol.clone())].clone();
                        if !cell.contains(&action) {
                            if cell.contains("r") {
                                cell.push_str("/");
                            }
                            cell.push_str(&action);
                        }
                    } else if body.last().unwrap().eq(".") && head.ne(&self.grammar.start) {
                    } else {
                    }
                }
            }
        }
    }

    pub fn parse(&self, input: &str) -> bool {
        false
    }
}

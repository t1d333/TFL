use std::{
    char,
    collections::{HashMap, HashSet},
};

use crate::grammar::Grammar;

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
    first: HashMap<String, HashSet<String>>,
    follow: HashMap<String, HashSet<String>>,
    action: Vec<String>,
    goto: Vec<String>,
    table_symbols: Vec<String>,
    items: Vec<HashMap<String, HashSet<Vec<String>>>>,
    indexed_grammar: Vec<(String, Vec<String>)>,
    table: HashMap<(usize, String), String>,
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

        let mut res = Self {
            first: HashMap::new(),
            follow: HashMap::new(),
            action: grammar.terminals.clone().into_iter().collect(),
            goto: grammar.non_terminals.clone().into_iter().collect(),
            grammar,
            table_symbols: vec![],
            items: vec![],
            indexed_grammar,
            table: HashMap::new(),
        };

        res.action.push("$".to_string());
        res.goto.retain(|symbol| symbol.ne(&res.grammar.start));

        res.table_symbols.extend(res.action.clone());
        res.table_symbols.extend(res.goto.clone());

        res.find_first_and_follow();

        res.generate_items();

        res.build_table();

        let automata = res.gen_parsing_automata();
        println!("PARSING AUTOMATA:");
        println!("{}\n", automata);
        res.print_table();
        return res;
    }

    fn union(&self, lhs: &HashSet<String>, rhs: &mut HashSet<String>) -> bool {
        let old_len = rhs.len();
        rhs.extend(lhs.clone());

        old_len != rhs.len()
    }

    fn print_table(&self) {
        println!("PARSING TABLE:");
        print!("{:<6}", "");
        for s in &self.table_symbols {
            print!("| {:<10}", s);
        }
        println!(
            "\n{:-<width$}",
            "",
            width = (self.table_symbols.len() + 1) * 12
        );
        for (i, _) in self.items.iter().enumerate() {
            print!("{:<5} |", i);
            for s in self.table_symbols.iter() {
                print!("{:<10} |", self.table[&(i, s.clone())]);
            }
            println!();
        }
        println!();
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

    fn build_table(&mut self) {
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
                        if !self.grammar.terminals.contains(symbol) {
                            continue;
                        }
                        let action = format!(
                            "s{}",
                            self.items
                                .iter()
                                .position(|j| j.eq(&self.goto(item, symbol)))
                                .unwrap()
                        );

                        let cell = table.get_mut(&(i, symbol.clone())).unwrap();
                        if !cell.contains(&action) {
                            if cell.contains("r") {
                                cell.push_str("/");
                            }
                            cell.push_str(&action);
                        }
                    } else if body.last().unwrap().eq(".") && head.ne(&self.grammar.start) {
                        for (j, (tmp_head, tmp_body)) in self.indexed_grammar.iter().enumerate() {
                            let mut body_without_dot = body.clone();
                            body_without_dot.pop();
                            if tmp_head.eq(head) && body_without_dot.eq(tmp_body) {
                                for f in &self.follow[head] {
                                    let cell = table.get_mut(&(i, f.clone())).unwrap();
                                    if cell.len() != 0 {
                                        cell.push_str("/");
                                    }
                                    cell.push_str(&format!("r{}", j));
                                }
                                break;
                            }
                        }
                    } else {
                        table.insert((i, "$".to_string()), "acc".to_string());
                    }
                }
            }
            for t in &self.grammar.non_terminals {
                let j = self.goto(item, t);
                if self.items.contains(&j) {
                    table.insert(
                        (i, t.clone()),
                        format!(
                            "{}",
                            self.items.iter().position(|item| item.eq(&j)).unwrap()
                        ),
                    );
                }
            }
        }
        self.table = table;
    }

    pub fn gen_parsing_automata(&self) -> String {
        let mut out = "digraph D {\n\tinit [label=\"\", shape=point]\n\tinit -> 0".to_string();
        for (i, item) in self.items.iter().enumerate() {
            let mut label = "[label=\"".to_string();
            for (head, bodies) in item {
                for body in bodies {
                    label = format!("{}{} -> {}\\n", label, head, body.join(""));
                }
            }
            label.push_str("\"]");
            out = format!("{}\n\t{} {}", out, i, label);
        }

        out = format!("{}\n\taccept [label=\"accept\", shape=doublecircle]", out);

        for (i, _) in self.items.iter().enumerate() {
            for symbol in self.table_symbols.iter() {
                if let Ok(n) = self.table[&(i, symbol.clone())].parse::<usize>() {
                    out = format!("{}\n\t{} -> {} [label=\"{}\"]", out, i, n, symbol);
                } else if self.table[&(i, symbol.clone())].contains("s") {
                    let cell = self.table[&(i, symbol.clone())].clone();
                    let pos = cell.chars().into_iter().position(|c| c.eq(&'s')).unwrap() + 1;
                    let mut chars: Vec<char> = cell.chars().into_iter().skip(pos).collect();
                    if chars.contains(&'/') {
                        let slash_pos = chars.iter().position(|c| c.eq(&'/')).unwrap();
                        chars = chars[..slash_pos].to_vec();
                    }

                    let state = chars
                        .iter()
                        .fold("".to_string(), |acc, c| format!("{}{}", acc, c));
                    if let Ok(n) = state.parse::<usize>() {
                        out = format!("{}\n\t{} -> {} [label=\"{}\"]", out, i, n, symbol);
                    }
                } else if self.table[&(i, symbol.clone())].eq("acc") {
                    out = format!("{}\n\t{} -> accept [label=\"{}\"]", out, i, "$");
                }
            }
        }

        format!("{}\n}}", out)
    }

    pub fn parse(&self, input: &str) -> bool {
        false
    }
}

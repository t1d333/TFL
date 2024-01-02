use std::{
    char,
    collections::{HashMap, HashSet},
    fmt::{format, write, Display},
    vec,
};

use crate::grammar::Grammar;
use crate::stack::Stack;

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
    table: HashMap<(usize, String), Vec<TableCell>>,
}

#[derive(Debug, Clone)]
enum TableCell {
    Reduce(usize),
    Shift(usize),
    NewState(usize),
    Accept,
}

impl Display for TableCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TableCell::Reduce(n) => write!(f, "r{}", n),
            TableCell::Shift(n) => write!(f, "s{}", n),
            TableCell::NewState(n) => write!(f, "{}", n),
            TableCell::Accept => write!(f, "acc"),
        }
    }
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

        return res;
    }

    fn union(&self, lhs: &HashSet<String>, rhs: &mut HashSet<String>) -> bool {
        let old_len = rhs.len();
        rhs.extend(lhs.clone());

        old_len != rhs.len()
    }

    pub fn print_table(&self) {
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
                let tmp = self.table[&(i, s.clone())]
                    .iter()
                    .fold("".to_string(), |acc, c| format!("{}{}/", acc, c));
                print!("{:<10} |", tmp);
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
                    let flag = self.union(
                        &first.get(&body[0].to_string()).unwrap().clone(),
                        first.get_mut(head).unwrap(),
                    );
                    updated = flag || updated;

                    let mut aux = follow[head].clone();
                    for symbol in body.iter().rev() {
                        if follow.contains_key(symbol) {
                            let flag = self.union(&aux, follow.get_mut(symbol).unwrap());
                            updated = updated || flag;
                        }

                        aux = first[symbol].clone();
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
        let mut table: HashMap<(usize, String), Vec<TableCell>> = HashMap::new();

        for i in 0..self.items.len() {
            for symbol in &self.table_symbols {
                table.insert((i, symbol.clone()), vec![]);
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

                        let new_state = self
                            .items
                            .iter()
                            .position(|j| j.eq(&self.goto(item, symbol)))
                            .unwrap();
                        let action = TableCell::Shift(new_state);

                        let cell = table.get_mut(&(i, symbol.clone())).unwrap();
                        if cell
                            .iter()
                            .position(
                                |item| matches!(item, TableCell::Shift(tmp) if *tmp == new_state),
                            )
                            .is_none()
                        {
                            cell.push(action);
                        }
                    } else if body.last().unwrap().eq(".") && head.ne(&self.grammar.start) {
                        for (j, (tmp_head, tmp_body)) in self.indexed_grammar.iter().enumerate() {
                            let mut body_without_dot = body.clone();
                            body_without_dot.pop();
                            if tmp_head.eq(head) && body_without_dot.eq(tmp_body) {
                                for f in &self.follow[head] {
                                    let cell = table.get_mut(&(i, f.clone())).unwrap();
                                    cell.push(TableCell::Reduce(j))
                                }
                                break;
                            }
                        }
                    } else {
                        table.insert((i, "$".to_string()), vec![TableCell::Accept]);
                    }
                }
            }
            for t in &self.grammar.non_terminals {
                let j = self.goto(item, t);
                if self.items.contains(&j) {
                    table.insert(
                        (i, t.clone()),
                        vec![TableCell::NewState(
                            self.items.iter().position(|item| item.eq(&j)).unwrap(),
                        )],
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
                if let Some(n) = self.table[&(i, symbol.clone())]
                    .iter()
                    .position(|c| matches!(c, TableCell::Accept))
                {
                    // self.table[&(i, symbol.clone())]
                    if let TableCell::NewState(n) = self.table[&(i, symbol.clone())][n] {
                        out = format!("{}\n\t{} -> {} [label=\"{}\"]", out, i, n, symbol);
                    }
                } else if let Some(n) = self.table[&(i, symbol.clone())]
                    .iter()
                    .position(|c| matches!(c, TableCell::Shift(..)))
                {
                    if let TableCell::Shift(n) = self.table[&(i, symbol.clone())][n] {
                        out = format!("{}\n\t{} -> {} [label=\"{}\"]", out, i, n, symbol);
                    }
                } else if self.table[&(i, symbol.clone())]
                    .iter()
                    .position(|c| matches!(c, TableCell::Accept))
                    .is_some()
                {
                    out = format!("{}\n\t{} -> accept [label=\"{}\"]", out, i, "$");
                }
            }
        }

        format!("{}\n}}", out)
    }

    fn do_shift(
        &self,
        state: usize,
        symbol: &str,
        stack_pos: usize,
        word_pointer: usize,
        stack: &mut Stack,
    ) {
        if let TableCell::Shift(new_state) = self.table[&(state, symbol.to_string())][0] {
            stack.push(symbol, new_state, stack_pos, word_pointer);
        }
    }

    fn do_reduce(
        &self,
        state: usize,
        symbol: &str,
        stack_pos: usize,
        word_pointer: usize,
        stack: &mut Stack,
    ) {
        if let TableCell::Reduce(rule_number) = self.table[&(state, symbol.to_string())][0] {
            let (head, body) = &self.indexed_grammar[rule_number];

            for _ in 0..(body.len()) {
                stack.pop(stack_pos);
            }
            let top = stack.top(stack_pos);
            if let TableCell::NewState(new_state) = self.table[&(top.0.state, head.clone())][0] {
                stack.push(head, new_state, stack_pos, word_pointer);
            }
        }
    }

    pub fn parse(&self, input: &str) -> Result<(), String> {
        let stream: Vec<String> = format!("{} $", input)
            .split(" ")
            .map(|s| s.to_string())
            .collect();
        let mut stack = Stack::new("", 0, 0);
        let mut step = 0;
        loop {
            println!("STACK STATE ON STEP №{} {}", step, stack);
            step += 1;
            let mut min_pos = 0;
            for i in stack.get_alived() {
                let curr_top = stack.top(i);
                let state = curr_top.0.state;
                let pos = curr_top.1;
                let symbol = stream[pos].clone();

                if min_pos == 0 || pos < min_pos {
                    min_pos = pos;
                }
                if self.grammar.terminals.get(&symbol).is_none() && symbol.ne("$") {
                    return Err(format!("Unrecognized symbol in position {}", pos));
                } else if self.table[&(state, symbol.to_string())].is_empty() {
                    stack.remove_root(i);
                    continue;
                } else if self.table[&(state, symbol.to_string())].len() > 1 {
                    if let Some(idx) = self.table[&(state, symbol.to_string())]
                        .iter()
                        .position(|c| matches!(c, TableCell::Shift(..)))
                    {
                        if let TableCell::Shift(shift_state) =
                            self.table[&(state, symbol.to_string())][idx]
                        {
                            let idx = stack.add_new_root(i);
                            stack.push(&symbol, shift_state, idx, pos + 1);
                        }
                    }

                    for item in self.table[&(state, symbol.to_string())].iter() {
                        if matches!(item, TableCell::Shift(..)) {
                            continue;
                        }

                        let idx = stack.add_new_root(i);
                        if let TableCell::Reduce(rule_number) = item.clone() {
                            let (head, body) = &self.indexed_grammar[rule_number];

                            for _ in 0..(body.len()) {
                                stack.pop(idx);
                            }

                            let top = stack.top(idx);
                            if let TableCell::NewState(new_state) =
                                self.table[&(top.0.state, head.clone())][0]
                            {
                                stack.push(head, new_state, idx, pos);
                            }
                        }
                    }
                    stack.remove_root(i);
                } else if let TableCell::Shift(_) = self.table[&(state, symbol.to_string())][0] {
                    self.do_shift(state, &symbol, i, pos + 1, &mut stack);
                } else if let TableCell::Reduce(_) = self.table[&(state, symbol.to_string())][0] {
                    self.do_reduce(state, &symbol, i, pos, &mut stack);
                } else if let TableCell::Accept = self.table[&(state, symbol.to_string())][0] {
                    return Ok(());
                }
            }
            if stack.get_alived().is_empty() {
                return Err(format!(
                    "Input is not recognized by this grammar; symbol position: {}",
                    min_pos
                ));
            }
        }
    }
}

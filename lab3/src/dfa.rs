use rand::Rng;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

#[derive(Debug)]
pub struct DFA {
    final_states: HashSet<i32>,
    transitions: HashMap<(i32, char), i32>,
    start_state: i32,
    symbols: Vec<char>,
}

impl DFA {
    pub fn new(
        final_states: &[i32],
        transitions: &HashMap<(i32, char), i32>,
        start_state: i32,
        symbols: &[char],
    ) -> Self {
        Self {
            final_states: HashSet::from_iter(final_states.to_owned()),
            start_state,
            transitions: transitions.clone(),
            symbols: symbols.to_owned(),
        }
    }

    fn is_trap(&self, state: i32) -> bool {
        for c in self.symbols.iter() {
            if matches!(self.transitions.get(&(state, *c)), Some(..)) {
                return false;
            }
        }

        true
    }

    pub fn validate_string(&self, w: &str) -> bool {
        let chars = w.chars();
        let mut curr_state = self.start_state;

        for c in chars {
            if let Some(next_state) = self.transitions.get(&(curr_state, c)) {
                curr_state = *next_state;
            } else {
                return false;
            }
        }

        self.final_states.contains(&curr_state)
    }

    pub fn add_transition(&mut self, lhs_state: i32, rhs_state: i32, symbol: char) {
        self.transitions.insert((lhs_state, symbol), rhs_state);
    }

    pub fn add_final_state(&mut self, state: i32) {
        self.final_states.insert(state);
    }

    pub fn gen_random_valid_word(&self, len: usize) -> String {
        let mut word = "".to_string();
        let mut state = self.start_state;
        let mut rng = rand::thread_rng();

        let mut stack = vec![];
        let mut visited = HashSet::new();

        loop {
            if self.final_states.contains(&state) && word.len() >= len {
                return word;
            }

            if self.is_trap(state) {
                state = *stack.last().unwrap();
                continue;
            }

            let mut r: f64 = rng.gen::<f64>();
            let mut index = (r * (self.symbols.len() as f64)).floor() as usize;

            while !self.transitions.contains_key(&(state, self.symbols[index])) {
                r = rng.gen::<f64>();
                index = (r * (self.symbols.len() as f64)).floor() as usize;
            }

            stack.push(state);
            visited.insert(state);
            word.push(self.symbols[index]);
            state = *self.transitions.get(&(state, self.symbols[index])).unwrap();
        }
    }
}

impl Display for DFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = "digraph D {\n\trankdir=LR\n\tnode [shape = circle];\n".to_string();

        let final_states = self
            .final_states
            .iter()
            .fold("".to_string(), |acc, s| {
                format!("{}\t{} [shape = doublecircle];\n", acc, s)
            });

        result = format!("{}\n{}", result, final_states);
        result = format!(
            "{}\n\tinit [label=\"\", shape=point]\n\tinit -> {}",
            result, self.start_state
        );


        for (k, v) in self.transitions.iter() {
            result = format!("{}\n\t{} -> {} [label = \"{}\"];", result, k.0, v, k.1);
        }

        write!(f, "{}\n}}", result)
    }
}

use itertools::Itertools;
use std::process::Command;

use crate::dfa::DFA;

pub trait MAT {
    fn membership(&self, w: &str) -> bool;
    fn equivalence(&self, automata: &DFA) -> Result<(), String>;
}

#[derive(Debug)]
pub struct ScriptMAT {
    oracle_src: String,
    symbols: Vec<char>,
    max_word_len: usize,
    try_count: usize,
}

impl ScriptMAT {
    pub fn new(oracle_src: &str, symbols: String, max_word_len: usize, try_count: usize) -> Self {
        Self {
            oracle_src: oracle_src.to_string(),
            max_word_len,
            symbols: symbols.chars().collect_vec(),
            try_count,
        }
    }

    fn get_permutations(&self, elements: &[char], k: usize) -> Vec<Vec<char>> {
        let mut result = Vec::new();
        self.generate_permutations(elements, k, Vec::new(), &mut result);
        result
    }

    fn generate_permutations(
        &self,
        elements: &[char],
        k: usize,
        current: Vec<char>,
        result: &mut Vec<Vec<char>>,
    ) {
        if k == 0 {
            result.push(current);
            return;
        }

        for element in elements {
            let mut new_current = current.clone();
            new_current.push(element.clone());
            self.generate_permutations(elements, k - 1, new_current, result);
        }
    }
}

impl MAT for ScriptMAT {
    fn membership(&self, w: &str) -> bool {
        let output = Command::new(&self.oracle_src).arg(w).output().unwrap();
        String::from_utf8(output.stdout).unwrap().eq("1\n")
    }

    fn equivalence(&self, automata: &DFA) -> Result<(), String> {
        let mut i = 0;
        let mut curr_len = 1;
        loop {
            if i > self.try_count || curr_len > self.max_word_len {
                break;
            }

            if i == 0 {
                if self.membership("") != automata.validate_string("") {
                    return Err("".to_string());
                }
                i += 1;
                curr_len += 1;
                continue;
            }

            let words = self.get_permutations(&self.symbols, curr_len);
            for word in &words {
                let word_str: String = word.into_iter().collect();
                if self.membership(&word_str) != automata.validate_string(&word_str) {
                    return Err(word_str);
                }
            }

            i += words.len();
            curr_len += 1;
        }

        Ok(())
    }
}

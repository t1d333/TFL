use std::process::Command;

use rand::Rng;

use crate::dfa::DFA;

pub trait MAT {
    fn membership(&self, w: &str) -> bool;
    fn equivalence(&self, automata: &DFA) -> Result<(), String>;
}

#[derive(Debug)]
pub struct ScriptMAT {
    oracle_src: String,
}

impl ScriptMAT {
    pub fn new(oracle_src: &str) -> Self {
        Self {
            oracle_src: oracle_src.to_string(),
        }
    }
}

impl MAT for ScriptMAT {
    fn membership(&self, w: &str) -> bool {
        let output = Command::new(&self.oracle_src).arg(w).output().unwrap();
        String::from_utf8(output.stdout).unwrap().eq("1\n")
    }

    fn equivalence(&self, automata: &DFA) -> Result<(), String> {
        for i in 1..17 {
            let k1 = rand::thread_rng().gen_range(0..i);
            let k2 = rand::thread_rng().gen_range(0..i);
            let word = format!("{}{}", "a".repeat(k1), "b".repeat(k2));
            if !automata.validate_string(&word) {
                return Err(word.to_string());
            }
        }
        Ok(())
    }
}

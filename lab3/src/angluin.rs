use std::collections::{HashMap, HashSet};

use crate::{dfa::DFA, mat::MAT};

struct AngluinWorker {
    symbols: Vec<char>,
    mat: Box<dyn MAT>,
    suffix_set: HashSet<String>,
    prefix_set: HashSet<String>,
    extended_prefix_set: HashSet<String>,
    table: HashMap<(String, String), bool>,
    extended_table: HashMap<(String, String), bool>,
}

impl AngluinWorker {
    fn new(symbols: &[char], mat: Box<dyn MAT>) -> Self {
        Self {
            symbols: symbols.to_owned(),
            mat,
            table: HashMap::new(),
            extended_table: HashMap::new(),
            suffix_set: HashSet::new(),
            prefix_set: HashSet::new(),
            extended_prefix_set: HashSet::new(),
        }
    }

    fn check_completeness(&self) -> Option<String> {
        for extended_pref in self.extended_prefix_set.iter() {
            let mut counter = self.prefix_set.len();
            for pref in self.prefix_set.iter() {
                for suff in self.suffix_set.iter() {
                    if self.extended_table[&(pref.to_string(), suff.to_string())]
                        != self.extended_table[&(extended_pref.to_string(), suff.to_string())]
                    {
                        counter -= 1;
                        break;
                    }
                }
            }
            if counter == 0 {
                return Some(extended_pref.clone());
            }
        }
        None
    }

    fn check_prefixes_consistency(
        &self,
        lhs: &String,
        rhs: &String,
        table: &HashMap<(String, String), bool>,
    ) -> Option<String> {
        for suff in self.suffix_set.iter() {
            if table[&(lhs.to_string(), suff.to_string())]
                != table[&(rhs.to_string(), suff.to_string())]
            {
                return Some(suff.clone());
            }
        }
        None
    }

    fn check_consistency(&self) -> Option<String> {
        for pref_u in self.extended_prefix_set.iter() {
            for pref_v in self.extended_prefix_set.iter() {
                if pref_v == pref_u || !pref_v.len() == pref_u.len() {
                    continue;
                }

                let l = pref_u.len();

                if pref_u.as_bytes()[l - 1] != pref_v.as_bytes()[l - 1] {
                    continue;
                }

                if self
                    .check_prefixes_consistency(
                        &pref_u[0..l - 1].to_string(),
                        &pref_v[0..l - 1].to_string(),
                        &self.table,
                    )
                    .is_some()
                {
                    continue;
                }

                if let Some(mut suff) =
                    self.check_prefixes_consistency(pref_u, pref_v, &self.extended_table)
                {
                    suff.insert(0, pref_v.chars().nth(l - 1).unwrap());
                    return Some(suff);
                }
            }
        }
        None
    }

    fn update_extended_table(&mut self) {
        self.extended_prefix_set.clear();
        self.extended_table.clear();

        self.prefix_set
            .clone()
            .into_iter()
            .filter(|pref| {
                self.prefix_set
                    .iter()
                    .all(|p| pref.eq(p) || !p.starts_with(pref))
            })
            .for_each(|pref| {
                for c in self.symbols.iter() {
                    let mut tmp = pref.clone();
                    tmp.push(*c);
                    self.extended_prefix_set.insert(tmp);
                }
            });

        for pref in self.extended_prefix_set.iter() {
            for suff in self.suffix_set.iter() {
                self.extended_table.insert(
                    (pref.to_string(), suff.to_string()),
                    self.mat.membership(&format!("{}{}", pref, suff)),
                );
            }
        }
    }

    fn update_table(&mut self) {
        for pref in self.prefix_set.iter() {
            for suff in self.suffix_set.iter() {
                self.table.insert(
                    (pref.to_string(), suff.to_string()),
                    self.mat.membership(&format!("{}{}", pref, suff)),
                );
            }
        }
    }

    pub fn run(&mut self) -> DFA {
        self.extended_table.clear();
        self.table.clear();
        self.suffix_set.clear();
        self.prefix_set.clear();
        self.extended_prefix_set.clear();

        self.suffix_set.insert("".to_string());
        self.prefix_set.insert("".to_string());
        self.table
            .insert(("".to_string(), "".to_string()), self.mat.membership(""));
        self.update_extended_table();

        loop {
            let mut completeness_breaker = None;
            let mut consistency_breaker = None;
            if let Some(pref) = self.check_completeness() {
                completeness_breaker = Some(pref);
            }

            if let Some(suff) = self.check_consistency() {
                consistency_breaker = Some(suff);
            }

            if completeness_breaker.is_none() && consistency_breaker.is_none() {
                let dfa = self.generate_dfa();
                let tmp = self.mat.equivalence(&dfa);
                match tmp {
                    Ok(_) => return dfa,
                    Err(contrexample) => {
                        for i in 1..contrexample.len() + 1 {
                            self.prefix_set.insert(contrexample[0..i].to_string());
                        }
                    }
                }
            } else {
                if let Some(pref) = completeness_breaker {
                    self.prefix_set.insert(pref);
                }

                if let Some(suff) = consistency_breaker {
                    self.suffix_set.insert(suff);
                }
            }

            self.update_table();
            self.update_extended_table();
        }
    }
	
    fn generate_dfa(&self) -> DFA {
        unimplemented!();
    }
}

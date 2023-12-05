#![feature(bitvec, bitset)]

use crate::{dfa::DFA, mat::MAT};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Debug)]
struct TableElem {
    val: String,
}

struct AngluinWorker {
    symbols: Vec<char>,
    mat: Box<dyn MAT>,
    suffix_set: BTreeSet<String>,
    prefix_set: BTreeSet<String>,
    extended_prefix_set: BTreeSet<String>,
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
            suffix_set: BTreeSet::new(),
            prefix_set: BTreeSet::new(),
            extended_prefix_set: BTreeSet::new(),
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

    fn add_new_prefix(&mut self, pref: &str) {
        self.prefix_set.insert(pref.to_string());
        for suf in self.suffix_set.iter() {
            self.table.insert(
                (pref.to_string(), suf.to_string()),
                self.mat.membership(&format!("{}{}", pref, suf)),
            );
        }
    }

    fn add_new_suffix(&mut self, suff: &str) {
        self.suffix_set.insert(suff.to_string());

        for pref in self.prefix_set.iter() {
            self.table.insert(
                (pref.to_string(), suff.to_string()),
                self.mat.membership(&format!("{}{}", pref, suff)),
            );
        }

        for pref in self.extended_prefix_set.iter() {
            self.extended_table.insert(
                (pref.to_string(), suff.to_string()),
                self.mat.membership(&format!("{}{}", pref, suff)),
            );
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
            let completeness_breaker = self.check_completeness();
            let consistency_breaker = self.check_consistency();
            if completeness_breaker.is_none() && consistency_breaker.is_none() {
                let dfa = self.generate_dfa();
                let tmp = self.mat.equivalence(&dfa);
                match tmp {
                    Ok(_) => return dfa,
                    Err(contrexample) => {
                        for i in 1..contrexample.len() + 1 {
                            self.add_new_prefix(&contrexample[0..i]);
                        }
                    }
                }
            } else {
                if let Some(pref) = completeness_breaker {
                    self.add_new_prefix(&pref);
                }

                if let Some(suff) = consistency_breaker {
                    self.add_new_suffix(&suff);
                }
            }
            self.update_extended_table();
        }
    }

    fn generate_dfa(&self) -> DFA {
        let mut result = DFA::new(&vec![], &HashMap::new(), 0, &self.symbols);
        let mut prefixes_sorted_by_len = self.prefix_set.iter().cloned().collect::<Vec<String>>();
        let mut states = HashMap::new();

        prefixes_sorted_by_len.sort_by(|lhs, rhs| lhs.len().cmp(&rhs.len()));

        let mut row_to_pref = HashMap::new();
        let mut pref_to_row = HashMap::new();

        let mut j = 0;
        for pref in prefixes_sorted_by_len.iter() {
            let mut row = fixedbitset::FixedBitSet::with_capacity(self.suffix_set.len());
            let mut i = 0;
            for suff in self.suffix_set.iter() {
                row.set(i, self.table[&(pref.to_owned(), suff.to_owned())]);
                i += 1;
            }

            if !row_to_pref.get(&row).is_none() {
                states.insert(pref, j);
                row_to_pref.insert(row.clone(), pref.clone());
                pref_to_row.insert(pref.clone(), row);
                j += 1;
            }
        }

        for (pref, i) in states.iter() {
            for c in self.symbols.iter() {
                let tmp = format!("{}{}", pref, c);
                if pref_to_row.get(&tmp).is_some() {
                    result.add_transition(*i, states[&tmp], *c);
                }
            }
        }

        return result;
    }
}

use crate::{dfa::DFA, mat::MAT};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
};

pub struct AngluinWorker {
    symbols: Vec<char>,
    mat: Box<dyn MAT>,
    suffix_set: BTreeSet<String>,
    prefix_set: BTreeSet<String>,
    extended_prefix_set: BTreeSet<String>,
    table: HashMap<(String, String), bool>,
    extended_table: HashMap<(String, String), bool>,
}

impl AngluinWorker {
    pub fn new(symbols: &[char], mat: Box<dyn MAT>) -> Self {
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
                    if self.table[&(pref.to_string(), suff.to_string())]
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
                let u_len = pref_u.len();
                let v_len = pref_v.len();

                if pref_v == pref_u
                    || pref_u.as_bytes()[u_len - 1] != pref_v.as_bytes()[v_len - 1]
                    || pref_u[0..u_len - 1] == pref_v[0..v_len - 1]
                {
                    continue;
                }

                if self
                    .check_prefixes_consistency(
                        &pref_u[0..u_len - 1].to_string(),
                        &pref_v[0..v_len - 1].to_string(),
                        &self.table,
                    )
                    .is_some()
                {
                    continue;
                }

                if let Some(mut suff) =
                    self.check_prefixes_consistency(pref_u, pref_v, &self.extended_table)
                {
                    suff.insert(0, pref_v.chars().nth(v_len - 1).unwrap());
                    return Some(suff);
                }
            }
        }
        None
    }

    fn update_extended_table(&mut self) {
        self.extended_table.clear();
        self.extended_prefix_set.clear();

        self.prefix_set.clone().into_iter().for_each(|pref| {
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

        if self.table[&("".to_string(), "".to_string())] {
            result.add_final_state(0);
        }

        let mut prefixes_sorted_by_len = self.prefix_set.iter().cloned().collect::<Vec<String>>();
        let mut suffixes_sorted_by_len = self.suffix_set.iter().cloned().collect::<Vec<String>>();
        let mut states = HashMap::new();

        prefixes_sorted_by_len.sort_by(|lhs, rhs| self.str_cmp(lhs, rhs));
        suffixes_sorted_by_len.sort_by(|lhs, rhs| self.str_cmp(lhs, rhs));

        let mut row_to_pref = HashMap::new();
        let mut pref_to_row = HashMap::new();

        let mut j = 0;

        for pref in prefixes_sorted_by_len.iter() {
            let row = self.get_row(pref, &suffixes_sorted_by_len, &self.table);
            if row_to_pref.get(&row).is_none() {
                states.insert(pref, j);
                row_to_pref.insert(row.clone(), pref.clone());
                pref_to_row.insert(pref, row);
                j += 1;
            }
        }

        for (pref, i) in states.iter() {
            for c in self.symbols.iter() {
                let tmp = format!("{}{}", pref, c);
                let row = self.get_row(&tmp, &suffixes_sorted_by_len, &self.extended_table);
                if let Some(p) = row_to_pref.get(&row) {
                    result.add_transition(*i, states[p], *c);
                }
            }
        }

        for (pref, i) in states.iter() {
            if self.table[&(pref.to_string(), "".to_string())] {
                result.add_final_state(*i);
            }
        }

        return result;
    }

    fn get_row(
        &self,
        pref: &str,
        suffixes: &Vec<String>,
        table: &HashMap<(String, String), bool>,
    ) -> fixedbitset::FixedBitSet {
        let mut row = fixedbitset::FixedBitSet::with_capacity(suffixes.len());
        for (i, suff) in suffixes.iter().enumerate() {
            if table.contains_key(&(pref.to_string(), suff.to_string())) {
                row.set(i, table[&(pref.to_string(), suff.to_string())]);
            } else {
                // let mut word = pref.to_string();
                // word.push_str(suff);
                // row.set(i, self.mat.membership(&word));
            }
        }
        return row;
    }

    fn str_cmp(&self, lhs: &str, rhs: &str) -> Ordering {
        if lhs.len() < rhs.len() {
            return Ordering::Less;
        } else if lhs.len() == rhs.len() {
            return lhs.cmp(rhs);
        } else {
            return Ordering::Greater;
        }
    }
}

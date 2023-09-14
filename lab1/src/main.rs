use std::collections::{HashMap, HashSet};
use std::{char, io};

fn main() {
    let rules = InputReader::read();
    let mut constants = HashSet::new();
    let mut smt = String::new();
    for (k, v) in &rules {
        match Solver::find_composition_coef(k) {
            Ok((rep, c)) => {
                constants.extend(c);
                println!("Left: {:#?}", rep);
            }
            Err(msg) => println!("{}", msg),
        }

        match Solver::find_composition_coef(v) {
            Ok((rep, c)) => {
                constants.extend(c);
                println!("Right: {:#?}", rep);
            }
            Err(msg) => println!("{}", msg),
        }
    }
}

#[derive(Debug)]
struct InputReader {}

impl InputReader {
    pub fn read() -> HashMap<String, String> {
        let mut result = HashMap::new();

        io::stdin().lines().for_each(|line| {
            let s = line.expect("failed to unwrap");
            let v: Vec<&str> = s.split("->").collect();
            result.insert(v[0].trim().to_string(), v[1].trim().to_string());
        });

        result
    }
}

struct Solver {}

impl Solver {
    pub fn find_composition_coef(
        compostion: &str,
    ) -> Result<(OrdinalRepresentation, HashSet<String>), String> {
        if compostion.chars().count() == 0 {
            return Err("length of composition is null".to_string());
        }

        let chars: Vec<char> = compostion.chars().collect();
        let mut constants: HashSet<String> = HashSet::new();
        let mut compostion_rep = OrdinalRepresentation::new(&chars[0].to_string());

        for c in chars.iter().skip(1) {
            let coefs = OrdinalRepresentation::new(&c.to_string());

            coefs.x_coefs.iter().for_each(|c| {
                constants.insert(c.clone());
            });

            coefs.free_coefs.iter().for_each(|c| {
                constants.insert(c.clone());
            });

            compostion_rep.apply_linear(&coefs);
        }

        Ok((compostion_rep, constants))
    }

    pub fn create_imparity(
        lhs: &mut OrdinalRepresentation,
        rhs: &mut OrdinalRepresentation,
        constants: &mut HashSet<String>,
    ) -> String {
        let mut res = String::from("(assert ");

        let (lhs_free_len, rhs_free_len) = (lhs.free_coefs.len(), rhs.free_coefs.len());

        for _ in lhs_free_len..rhs_free_len {
            lhs.free_coefs.push("0".to_string());
        }

        for _ in rhs_free_len..lhs_free_len {
            rhs.free_coefs.push("0".to_string());
        }

        res + ")"
    }
}

#[derive(Debug)]
struct OrdinalRepresentation {
    pub x_coefs: Vec<String>,
    pub free_coefs: Vec<String>,
}

impl OrdinalRepresentation {
    fn new(func_name: &str) -> Self {
        Self {
            x_coefs: vec![format!("a2{}", func_name), format!("a1{}", func_name)],
            free_coefs: vec![format!("b2{}", func_name), format!("b1{}", func_name)],
        }
    }

    pub fn apply_linear(&mut self, f: &OrdinalRepresentation) {
        let mut len = self.x_coefs.len();
        let max_x_coef = self.x_coefs[len - 1].clone();

        // update x coefs
        self.x_coefs.push(f.x_coefs[1].clone());
        len += 1;
        let n = self.x_coefs.get_mut(len - 2).unwrap();
        *n = format!("(* {} {})", max_x_coef, f.x_coefs[0]);

        // update free coefs
        self.free_coefs.push(f.free_coefs[1].clone());
        let n = self.free_coefs.get_mut(len - 2).unwrap();
        *n = format!("(+ (* {} {}) {})", max_x_coef, f.free_coefs[0], n);
    }
}

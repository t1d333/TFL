use std::collections::{HashSet, HashMap};

use crate::{ordinal::Representation, utils::smt};

pub fn find_composition_coef(
    compostion: &str,
) -> Result<(Representation, HashSet<String>), String> {
    if compostion.chars().count() == 0 {
        return Err("length of composition is null".to_string());
    }

    let chars: Vec<char> = compostion.chars().collect();
    let mut constants: HashSet<String> = HashSet::new();
    let mut compostion_rep = Representation::new(&chars[0].to_string());

    chars.iter().enumerate().for_each(|(i, c)| {
        let coefs = Representation::new(&c.to_string());

        coefs.x_coefs.iter().for_each(|c| {
            constants.insert(c.clone());
        });

        coefs.free_coefs.iter().for_each(|c| {
            constants.insert(c.clone());
        });

        if i == 0 {
            compostion_rep = coefs;
        } else {
            compostion_rep.apply_linear(&coefs);
        }
    });

    Ok((compostion_rep, constants))
}

pub fn generate_solution(rules: &HashMap<String, String>) -> Result<String, String> {
    let mut constants = HashSet::new();
    let mut code = "".to_string();
    let mut inequalities = "".to_string();

    for (k, v) in rules {
        let (lhs, lhs_coefs) = find_composition_coef(k)?;
        let (rhs, rhs_coefs) = find_composition_coef(v)?;

        constants.extend(lhs_coefs.into_iter());
        constants.extend(rhs_coefs.into_iter());

        let strict_by_x = format!(
            "(and {} {})",
            smt::Converter::create_inequality(&lhs.x_coefs, &rhs.x_coefs, true),
            smt::Converter::create_inequality(&lhs.free_coefs, &rhs.free_coefs, false)
        );

        let strict_by_free = format!(
            "(and {} {})",
            smt::Converter::create_inequality(&lhs.x_coefs, &rhs.x_coefs, false),
            smt::Converter::create_inequality(&lhs.free_coefs, &rhs.free_coefs, true)
        );

        let res = format!("(assert (or {} {}))", strict_by_x, strict_by_free);

        inequalities = format!("{}\n{}", inequalities, res);
        code = format!("{}\n{}", code, inequalities)
    }

    code = format!(
        "{}\n{}\n",
        &smt::Converter::define_constants(&constants),
        code
    );

    Ok(code)
}

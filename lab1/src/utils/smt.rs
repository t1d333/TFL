use std::collections::HashSet;

pub struct Converter {}

impl Converter {
    pub fn create_inequality(lhs: &Vec<String>, rhs: &Vec<String>, strict: bool) -> String {
        let null = "0".to_string();
        let mut result = "(or".to_string();
        let mut acc = "".to_string();
        let max_len = std::cmp::max(lhs.len(), rhs.len());

        (0..max_len).rev().for_each(|i| {
            let lhs_c = lhs.get(i).unwrap_or(&null);
            let rhs_c = rhs.get(i).unwrap_or(&null);
            if i > 0 {
                if acc.is_empty() {
                    result.push_str(&format!(" (and {} (> {} {}))", acc, lhs_c, rhs_c));
                    acc = format!("(and {} (= {} {}))", acc, lhs_c, rhs_c);
                } else {
                    result.push_str(&format!(" (> {} {})", lhs_c, rhs_c));
                    acc = format!("(= {} {}) ", lhs_c, rhs_c);
                }
            } else if strict {
                if !acc.is_empty() {
                    result.push_str(&format!(" (and {} (> {} {}))", acc, lhs_c, rhs_c));
                } else {
                    result.push_str(&format!(" (> {} {})", lhs_c, rhs_c));
                }
            } else if acc.is_empty() {
                result.push_str(&format!(" (and {} (>= {} {}))", acc, lhs_c, rhs_c));
            } else {
                result.push_str(&format!(" (>= {} {})", lhs_c, rhs_c));
            }
        });

        result + ")"
    }

    pub fn define_constants(constants: &HashSet<String>) -> String {
        let mut result = "".to_string();
        constants.iter().for_each(|c| {
            result = format!(
                "{}(declare-fun {} () Int)\n(assert (>= {} 0))\n",
                result, c, c
            );
        });

        result
    }
}

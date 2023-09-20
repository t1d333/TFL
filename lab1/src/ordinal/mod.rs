pub mod ordinal;

#[derive(Debug)]
pub struct Representation {
    pub x_coefs: Vec<String>,
    pub free_coefs: Vec<String>,
}

impl Representation {
    pub fn new(func_name: &str) -> Self {
        Self {
            x_coefs: vec![format!("a2{}", func_name), format!("a1{}", func_name)],
            free_coefs: vec![format!("b2{}", func_name), format!("b1{}", func_name)],
        }
    }

    pub fn apply_linear(&mut self, f: &Representation) {
        let mut len = self.x_coefs.len();
        let max_x_coef = self.x_coefs[len - 1].clone();

        // update x coefs
        self.x_coefs.push(f.x_coefs[1].clone());
        len += 1;
        let n = self.x_coefs.get_mut(len - 2).unwrap();
        *n = format!("(* {} {}) ", max_x_coef, f.x_coefs[0]);

        // update free coefs
        self.free_coefs.push(f.free_coefs[1].clone());
        let n = self.free_coefs.get_mut(len - 2).unwrap();
        *n = format!("(+ (* {} {}) {}) ", max_x_coef, f.free_coefs[0], n);
    }
}

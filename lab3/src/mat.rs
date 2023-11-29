use crate::dfa::DFA;

// #[derive(Debug)]
// pub struct MAT {
//     oracle_src: String,
// }

pub trait MAT {
    fn membership(&self, w: &str) -> bool;
    fn equivalence(&self, automata: &DFA) -> Result<(), String>;
}

// impl MAT {
//     pub fn new(oracle_src: &str) -> Self {
//         Self {
//             oracle_src: oracle_src.to_string(),
//         }
//     }
//
//     pub fn membership(&self, w: &str) -> bool {
//         let output = Command::new(&self.oracle_src).arg(w).output().unwrap();
//         String::from_utf8(output.stdout).unwrap().eq("1")
//     }
//
// 	// pub fn
//
//     pub fn equivalence(&self, automata: &DFA) -> Result<(), String> {
//         // let words = vec![];
//         // let output = Command::new(self.oracle_src)..output().unwrap();
//         // return None;
//         // output.eq("1")
//
// 		return Ok(())
//     }
// }

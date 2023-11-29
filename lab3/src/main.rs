use std::collections::HashMap;

use dfa::DFA;
use mat::MAT;

pub mod angluin;
pub mod dfa;
pub mod mat;

fn main() {
    let mut t = HashMap::new();

    t.insert((0, 'b'), 1);
    t.insert((0, 'a'), 0);
    t.insert((1, 'b'), 1);

    let d = DFA::new(&[1], &t, 0, &['a', 'b', 'c']);

    for _ in 0..10 {
        println!("{}", d.gen_random_valid_word(10));
    }
}

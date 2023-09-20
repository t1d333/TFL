use std::io;

mod ordinal;
mod solver;
mod utils;

fn main() {
    let stdio = io::stdin();
    let rules = utils::input_reader::Reader::read(stdio.lock());
    let smt_solution = solver::generate_solution(&rules).unwrap();
    println!("{}", smt_solution);
}

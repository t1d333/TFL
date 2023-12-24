use clap::Parser;
use mat::ScriptMAT;
pub mod angluin;
pub mod dfa;
pub mod mat;

#[derive(Parser)]
pub struct Cli {
    // Alphabet of the language specified by the oracle
    symbols: String,
    // Path to oracle
    oracle: String,
}

fn main() {
    let cli = Cli::parse();

    let mat = ScriptMAT::new(&cli.oracle);
    let symbols = cli.symbols.chars().collect::<Vec<char>>();

    let mut worker = angluin::AngluinWorker::new(&symbols, Box::new(mat));
    let dfa = worker.run();
    println!("{}", dfa);
}

use clap::{Parser, ValueHint};
use clap_complete::Shell;

#[derive(Parser, Debug)]
pub struct Completions {
    #[arg(long = "print-completion")]
    pub shell: Shell,
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Args {
    /// Path to the input file
    #[arg(value_hint = ValueHint::FilePath)]
    pub path: String,
}

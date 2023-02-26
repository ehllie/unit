use clap::{ArgGroup, Parser, ValueHint};
use clap_complete::Shell;

#[derive(Parser, Debug)]
pub struct Completions {
    #[arg(long = "print-completion")]
    pub shell: Shell,
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
#[command(group( ArgGroup::new("input").required(true)))]
pub struct Args {
    /// Path to the input file
    #[arg(
        value_hint = ValueHint::FilePath,
        default_value_t = String::new(),
        group = "input"
    )]
    pub path: String,

    /// Expression to evaluate
    #[arg(short, long, group = "input")]
    pub expr: Option<String>,
}

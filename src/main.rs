mod cli;
use clap::{CommandFactory, Parser as _};
use clap_complete::{generate, Generator};
use cli::{Args, Completions};
use std::fs::read_to_string;
mod parser;
use chumsky::Parser;
use parser::unit_parser;

fn main() {
    if let Ok(Completions { shell }) = Completions::try_parse() {
        print_completion(shell);
    } else {
        run(Args::parse());
    }
}

fn print_completion(gen: impl Generator) {
    let mut cmd = Args::command();
    let name = cmd.get_name().to_string();
    generate(gen, &mut cmd, name, &mut std::io::stdout())
}

fn run(args: Args) {
    let input = args
        .expr
        .unwrap_or_else(|| read_to_string(args.path).expect("could not read input file"));
    let ast = unit_parser().parse(input);
    println!("{ast:?}");
}

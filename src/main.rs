mod cli;
use clap::{CommandFactory, Parser};
use clap_complete::{generate, Generator};
use cli::{Args, Completions};

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
    println!("{args:?}");
}

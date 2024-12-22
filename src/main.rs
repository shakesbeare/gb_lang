use anyhow::Result;
use clap::Parser;
use gb_lang::*;
use tracing_subscriber::fmt::format;

#[derive(Debug, Parser)]
struct Cli {
    #[arg(help = "Run this file as a Gb program. If not present, enters REPL mode")]
    filename: Option<std::path::PathBuf>,
}

fn main() -> Result<()> {
    #[cfg(debug_assertions)]
    tracing_subscriber::fmt()
        .event_format(
            format()
                .with_thread_names(true)
                .with_thread_names(false)
                .with_target(false)
                .without_time()
                .pretty(),
        )
        .with_max_level(tracing::Level::INFO)
        .init();
    #[cfg(not(debug_assertions))]
    tracing_subscriber::fmt()
        .event_format(format().pretty())
        .with_max_level(tracing::Level::WARN)
        .init();

    let args = Cli::parse();

    match args.filename {
        Some(f) => {
            let input = std::fs::read_to_string(f)?;
            let mut i = interpreter::Interpreter::new(interpreter::TreeWalking::default(), input)?;
            i.evaluate();
            Ok(())
        }
        None => {
            println!("REPL mode");
            println!("It currently sucks");
            println!("-----------------------");
            repl()
        }
    }
}

fn repl() -> Result<()> {
    let mut i = interpreter::Interpreter::new_lazy(interpreter::TreeWalking::default());
    let mut buf = String::new();
    let stdin = std::io::stdin();
    loop {
        stdin.read_line(&mut buf)?;
        match i.new_input(&buf) {
            Ok(_) => {
                let result = i.evaluate();
                if result.is_gb_none() {
                    continue;
                }
                println!("{}", result);
            }
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
    }
}

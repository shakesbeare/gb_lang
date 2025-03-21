use std::io::Write;

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
            let _ = i.evaluate();

            Ok(())
        }
        None => {
            println!("Gb v{}", env!("CARGO_PKG_VERSION"));
            println!("―――――――――――――――――――――――");
            repl()
        }
    }
}

#[derive(Debug)]
enum Delimiter {
    Paren,
    Brace,
    Bracket,
}

impl From<Delimiter> for char {
    fn from(val: Delimiter) -> Self {
        match val {
            Delimiter::Paren => ')',
            Delimiter::Brace => '}',
            Delimiter::Bracket => ']',
        }
    }
}

fn repl() -> Result<()> {
    let mut i = interpreter::Interpreter::new_lazy(
        interpreter::TreeWalking::default(),
        gb_lang::error::REPLErrorHandler::default(),
    );
    let mut buf = String::new();
    let mut delimiters: Vec<Delimiter> = vec![];

    let stdin = std::io::stdin();
    loop {
        buf.clear();
        delimiters.clear();
        print!(">>> ");
        std::io::stdout().flush().unwrap();
        stdin.read_line(&mut buf)?;
        let mut balanced = check_balanced(&buf, &mut delimiters);
        // TODO: each "section" needs to be parsed as it's completed
        // rather than waiting for the entire block to be balanced
        while !delimiters.is_empty() {
            if balanced.is_err() {
                // this means that an unexpected closer occurred
                // we can rely on the parsing step to emit
                // a nice error message
                break;
            }
            let depth = delimiters.len();
            let tabs: String = "    ".repeat(depth);
            print!("... {}", tabs);
            std::io::stdout().flush().unwrap();
            stdin.read_line(&mut buf)?;
            delimiters.clear();
            balanced = check_balanced(&buf, &mut delimiters);
        }
        i.new_input(&buf);
        match parser::quick_parse(&buf) {
            Ok(ast) => {
                while !delimiters.is_empty() {}
                if let Ok(v) = i.eval_ast(&ast, false) {
                    if v.is_gb_none() {
                        continue;
                    }
                    println!("{}", v);
                }
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}

fn check_balanced<S: AsRef<str>>(input: S, delimiters: &mut Vec<Delimiter>) -> Result<(), char> {
    for c in input.as_ref().chars() {
        match c {
            '(' => delimiters.push(Delimiter::Paren),
            '{' => delimiters.push(Delimiter::Brace),
            '[' => delimiters.push(Delimiter::Bracket),
            c if [')', '}', ']'].contains(&c) => check_matches(c, delimiters)?,
            _ => (),
        }
    }

    Ok(())
}

fn check_matches(c: char, delimiters: &mut Vec<Delimiter>) -> Result<(), char> {
    let last = delimiters.pop();
    match last {
        Some(delim) => {
            let delim = delim.into();
            if c == delim {
                Ok(())
            } else {
                Err(c)
            }
        }
        None => Err(c),
    }
}

mod lexer;
mod token;

use anyhow::Result;
use clap::Parser as ClapParser;
use std::io;

#[derive(clap::Parser)]
struct Cli {
    #[arg(short = 'i', long = "interactive")]
    interactive_mode: bool,

    path: Option<std::path::PathBuf>,
}

fn main() -> Result<()> {
    let args = Cli::parse();


    let mut lexer = lexer::Lexer::new("file.gb")?;

    lexer.lex_all();

    for (l, t) in lexer.lexeme_stream.iter().zip(lexer.token_stream) {
        println!("{}: {:?}", l, t);
    }

    Ok(())
}


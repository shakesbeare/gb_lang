mod lexer;
mod token;

use anyhow::Result;
use clap::Parser as ClapParser;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Read};

use regex::Regex;

#[derive(clap::Parser)]
struct Cli {
    #[arg(short = 'i', long = "interactive")]
    interactive_mode: bool,

    path: Option<std::path::PathBuf>,
}

fn main() -> Result<()> {
    let args = Cli::parse();

    if args.interactive_mode {
        interactive_mode()?;
    }

    let lexemes = lex("file.gb");
    dbg!(lexemes);

    Ok(())
}

fn interactive_mode() -> Result<()> {
    for line in io::stdin().lock().lines() {
        match line {
            Ok(val) => {
                dbg!(val);
                ()
            }
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}

fn tokenize(lexemes: Vec<String>) {
    let lex_iter = lexemes.into_iter().enumerate();
    for (i, lexeme) in lex_iter {
        match lexeme {
            _ => {}
        }
    }
}

fn lex(file: &str) -> Result<Vec<String>> {
    let f = File::open(file)?;
    let mut reader = BufReader::new(f);
    let mut buf = [0; 1];
    let mut word = String::new();
    let mut lexemes = vec![];

    'outer: while let Ok(bytes_read) = reader.read(&mut buf[..]) {
        if bytes_read == 0 {
            break;
        }
        let char_read = buf[0] as char;

        if !char_read.is_alphanumeric() {
            // split word
            lexemes.push(word);
            match char_read {
                '/' => {
                    let Ok(bytes_read) = reader.read(&mut buf) else {
                        panic!("An error occured while checking for comment")
                    };

                    if bytes_read == 0 {
                        break;
                    }

                    let char_read = buf[0] as char;
                    if char_read == '/' {
                        // ignore until newline
                        'inner: while let Ok(bytes_read) = reader.read(&mut buf[..]) {
                            if bytes_read == 0 {
                                break 'outer;
                            }
                            let char_read = buf[0] as char;
                            if char_read == '\n' {
                                break 'inner;
                            }
                        } // end inner 
                    } // end if
                } // end '/'
                _ => {
                    lexemes.push(char_read.to_string());
                }
            } // end match char_read

            word = String::from("");
            continue;
        }

        word.push(char_read);
    }

    return Ok(lexemes);
}

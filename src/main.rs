mod lexer;
mod token;

use anyhow::Result;
use clap::Parser as ClapParser;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::fs::File;
use std::io::Read;

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
    
    let f = File::open("file.gb")?;
    let mut reader = BufReader::new(f);
    let mut buf = [0; 1];
    let mut str = String::new();

    while let Ok(bytes_read) = reader.read(&mut buf[..]) {
        if bytes_read == 0 { break; }
        str += &String::from_utf8_lossy(&buf);
        println!("{}", &str);
    }


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

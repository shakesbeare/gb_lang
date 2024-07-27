use anyhow::Result;
use gb_lang::*;

fn main() -> Result<()> {
    let input = std::fs::read_to_string("file.gb")?;
    let mut i = interpreter::Interpreter::new(interpreter::TreeWalking::default(), input)?;
    i.evaluate();
    Ok(())
}

use anyhow::Result;
use gb_lang::*;

fn main() -> Result<()> {
    #[cfg(debug_assertions)]
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .init();
    #[cfg(not(debug_assertions))]
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::WARN)
        .init();

    let input = std::fs::read_to_string("file.gb")?;
    let mut i = interpreter::Interpreter::new(interpreter::TreeWalking::default(), input)?;
    i.evaluate();
    Ok(())
}

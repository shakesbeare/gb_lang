use anyhow::Result;
use gb_lang::*;
use tracing_subscriber::fmt::format;

fn main() -> Result<()> {
    #[cfg(debug_assertions)]
    tracing_subscriber::fmt()
        .event_format(format()
            .with_thread_names(true)
            .with_thread_names(false)
            .with_target(false)
            .without_time()
            .pretty())
        .with_max_level(tracing::Level::TRACE)
        .init();
    #[cfg(not(debug_assertions))]
    tracing_subscriber::fmt()
        .event_format(format().pretty())
        .with_max_level(tracing::Level::WARN)
        .init();

    let input = std::fs::read_to_string("file.gb")?;
    let mut i = interpreter::Interpreter::new(interpreter::TreeWalking::default(), input)?;
    i.evaluate();
    Ok(())
}

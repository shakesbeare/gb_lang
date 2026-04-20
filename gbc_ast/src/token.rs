use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use gbc_lex::{lexer::Lexer, TokenType};
use gbc_shared::{Location, Span};

/// A token which is no longer reliant on the lifetime of the input and contains a little more
/// information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RichToken {
    Valid {
        /// The entire line where the token appears
        context: Arc<str>,
        literal: Arc<str>,
        ty: TokenType,
        /// The location of the token in the file
        span: Span,
    },
    Invalid {
        context: Arc<str>,
        message: Arc<str>,
        location: Location,
    },
}

pub struct TokenStream {
    stream: Vec<RichToken>,
}

impl<'input> From<Lexer<'input>> for TokenStream {
    fn from(value: Lexer<'input>) -> Self {
        let mut stream = Vec::new();
        let mut lines: HashMap<usize, Arc<str>> = HashMap::new();
        let input = value.input.split("\n");

        for token in value {
            match token {
                Ok(t) => {
                    let line = t.location.line;
                    let len = t.literal.len();
                    let entry = match lines.entry(line) {
                        Entry::Occupied(occupied_entry) => occupied_entry,
                        Entry::Vacant(vacant_entry) => {
                            let context: Arc<str> = Arc::from(input.clone().nth(line).unwrap());
                            vacant_entry.insert_entry(Arc::clone(&context))
                        }
                    };

                    stream.push(RichToken::Valid {
                        context: Arc::clone(entry.get()),
                        literal: Arc::from(t.literal),
                        ty: t.ty,
                        span: Span::new(t.location.offset, t.location.offset + len),
                    });
                }
                Err(e) => {
                    let message = Arc::from(e.msg);
                    let line = e.location.line;
                    let entry = match lines.entry(line) {
                        Entry::Occupied(occupied_entry) => occupied_entry,
                        Entry::Vacant(vacant_entry) => {
                            let context: Arc<str> = Arc::from(input.clone().nth(line).unwrap());
                            vacant_entry.insert_entry(Arc::clone(&context))
                        }
                    };

                    stream.push(RichToken::Invalid {
                        context: Arc::clone(entry.get()),
                        message,
                        location: e.location,
                    })
                }
            }
        }

        Self { stream }
    }
}

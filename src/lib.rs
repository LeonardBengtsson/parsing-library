//! # Examples
//! ```rust
//! use parsable::*;
//!
//! struct TestStruct {
//!     a: CharLiteral<b'a'>,
//!     b: CharLiteral<b'b'>,
//! }
//!
//! impl<'a> Parsable<'a> for TestStruct {
//!     fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self>
//!     where
//!         Self: Sized
//!     {
//!         stream.scope(|stream| {
//!             Some(Ok(TestStruct {
//!                 a: ok_or_throw!(CharLiteral::<b'a'>::parse(stream)?),
//!                 b: ok_or_throw!(CharLiteral::<b'b'>::parse_or_error(stream)),
//!             }))
//!         })
//!     }
//!
//!     fn error() -> ParseError {
//!         String::from("TestStruct")
//!     }
//! }
//!
//! enum TestEnum {
//!     X {
//!         a: CharLiteral<b'a'>,
//!         b: CharLiteral<b'b'>,
//!     },
//!     Y(TestStruct),
//! }
//!
//! impl<'a> Parsable<'a> for TestEnum {
//!     fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self>
//!     where
//!         Self: Sized 
//!     {
//!         stream.scope(|stream| {
//!             None
//!                 .or_else(|| Some(Ok(TestEnum::X {
//!                     a: ok_or_throw!(CharLiteral::<b'a'>::parse(stream)?),
//!                     b: ok_or_throw!(CharLiteral::<b'b'>::parse_or_error(stream)),
//!                 })))
//!                 .or_else(|| Some(Ok(TestEnum::Y (
//!                     ok_or_throw!(TestStruct::parse(stream)?),
//!                 ))))
//!         })
//!     }
//!
//!     fn error() -> ParseError {
//!         String::from("TestEnum")
//!     }
//! }
//! ```

pub mod stream;
pub mod parsable_types;

pub use crate::stream::*;
pub use crate::parsable_types::*;
pub use parsable_derive::Parsable;

use std::fmt::Debug;
use serde::{Deserialize, Serialize};

// a value of `None` symbolizes no match
// a value of `Some(Err(_))` symbolizes a partial match with an error that should be propagated
// a value of `Some(Ok(_))` symbolizes a complete match
pub type ParseOutcome<T> = Option<ParseResult<T>>;

pub type ParseResult<T> = Result<T, ParseErrorStack>;

pub type ParseErrorStack = Vec<ParseErrorContext>;

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct ParseErrorContext {
    pub error: ParseError,
    pub source_position: usize,
}

pub type ParseError = String;

pub trait Parsable<'a> {
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self>
    where
        Self: Sized;

    fn parse_or_error(stream: &mut ScopedStream<'a>) -> ParseResult<Self>
    where
        Self: Sized
    {
        Self::parse(stream)
            .unwrap_or_else(|| Err(vec![stream.error_here::<Self>()]))
    }

    /// Returns the error representing a missing value of this type. Should be of the form
    /// "<expected value>" such that the error can be formatted into "expected Identifier", for 
    /// example. For most types, this function should simply return the type name, or call the 
    /// error function of an inner value.
    // rust doesn't yet support const string formatting or generic parameters in static context,
    // so we have to make this a non-const runtime function that returns an owned string
    fn error() -> ParseError;

    /// Optionally returns an error to add to the error stack when propagating another error.
    /// Defaults to `Some(Self::error())`.
    fn propagated_error() -> Option<ParseError> {
        Some(Self::error())
    }
}

#[macro_export]
macro_rules! ok_or_throw {
    ($x:expr) => {
        match $x {
            Ok(ok) => ok,
            Err(err) => return Some(Err(err)),
        }
    }
}

pub fn format_error_stack(buffer: &[u8], stack: ParseErrorStack) -> String {
    let mut line_offsets = vec![0];
    let mut previous_line_offset = 0;
    let mut max_line_width = 0;
    let mut stream = ScopedStream::new(buffer);
    while !stream.at_end() {
        if matches!(stream.read(2, |chars| chars == b"\r\n"), Some(_)) ||
            matches!(stream.read(1, |chars| chars == b"\n"), Some(_)) ||
            matches!(stream.read(1, |chars| chars == b"\r"), Some(_))
        {
            let offset = stream.index();
            max_line_width = max_line_width.max(offset - previous_line_offset - 1);
            line_offsets.push(offset);
            previous_line_offset = offset;
        } else {
            stream.read(1, |_| true);
        }
    }

    let line_text_width = line_offsets.len().to_string().len();
    let column_text_width = max_line_width.to_string().len();

    let mut out = String::new();
    for ParseErrorContext { error, source_position } in stack.iter().rev() {
        let mut current_line = 0;
        let mut current_line_offset = 0;
        for (line, line_offset) in (0usize..).into_iter().zip(&line_offsets) {
            if line_offset > source_position {
                break;
            }
            current_line = line;
            current_line_offset = *line_offset;
        }
        let column = source_position - current_line_offset;

        out.push_str(&format!(
            "\nat {:>line_text_width$}:{:>column_text_width$}: Expected {}, found \"{}\"...",
            current_line + 1, column + 1, error,
            String::from_utf8_lossy(&buffer[*source_position..(source_position + 16).min(buffer.len())])
        ));
    }
    out
}


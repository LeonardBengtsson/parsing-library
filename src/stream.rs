use crate::{ParseErrorContext, ParseOutcome, Parsable};

pub struct ScopedStream<'a> {
    buffer: &'a [u8],
    index: usize,
}

impl<'a> ScopedStream<'a> {
    pub fn new(buffer: &'a [u8]) -> ScopedStream<'a> {
        ScopedStream { buffer, index: 0 }
    }

    pub fn at_end(&self) -> bool {
        self.index == self.buffer.len()
    }

    pub fn read<'c>(
        &'c mut self,
        len: usize,
        pred: impl FnOnce(&'c [u8]) -> bool)
        -> Option<&'c [u8]> 
    {
        if self.buffer.len() < len + self.index { return None; }

        let requested_slice = &self.buffer[self.index..(self.index + len)];

        pred(requested_slice).then_some({
            self.index += len;
            requested_slice
        })
    }

    pub fn scope<T: Parsable<'a>>(
        &mut self, 
        parse_fn: impl for<'b> FnOnce(&'b mut ScopedStream) -> ParseOutcome<T>) 
        -> ParseOutcome<T>
    {
        self.scope_with_span(parse_fn).map(
            |result| result.map(
                |(parsed_item, _)| parsed_item))
    }

    pub fn scope_with_span<T: Parsable<'a>>(
        &mut self, 
        parse_fn: impl for<'b> FnOnce(&'b mut ScopedStream) -> ParseOutcome<T>) 
        -> ParseOutcome<(T, &'a [u8])>
    {
        let start_index = self.index;

        let outcome = parse_fn(self);

        if matches!(outcome, None) {
            // reset index to before starting to parse this item
            self.index = start_index;
        }

        outcome.map(|result| {
            #[cfg(feature = "parsable_debug")] {
                if matches!(result, Ok(..)) {
                    println!("DEBUG BUFFER:\n{}$EOS", String::from_utf8_lossy(&self.buffer[self.index..]));
                }
            }
            let result = result.map(|parsed_item| {
                (parsed_item, &self.buffer[start_index..self.index])
            });
            result.map_err(|mut error_stack| {
                if let Some(propagated_error) = T::propagated_error() {
                    error_stack.push(ParseErrorContext {
                        error: propagated_error,
                        source_position: start_index,
                    });
                }
                error_stack
            })
        })
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn error_here<T: Parsable<'a>>(&self) -> ParseErrorContext {
        ParseErrorContext {
            error: T::error(),
            source_position: self.index,
        }
    }

    pub fn error_literal_value_here(&self, literal: &'static [u8]) -> ParseErrorContext {
        ParseErrorContext {
            error: format!("\"{}\"", String::from_utf8_lossy(literal)),
            source_position: self.index,
        }
    }
}

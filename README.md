# parsable

An easy-to-use declarative parsing library written in Rust. Easy-to-use in that usage optimally consists only of derived trait implementations, and declarative in that the parsing behavior is declared and implied by the type system. This means that the format declaration also defines the output type.

## Usage

This crate exposes the `Parsable` trait. Simply derive it using
`#[derive(Parsable)]` like this...

```rust
use parsable::*;

#[derive(Parsable)]
struct TestStruct {
    a: CharLiteral<b'a'>,
    b: CharLiteral<b'b'>,
}

fn main(source: &[u8]) {
    let mut stream = ScopedStream::new(source);
    let outcome = WithEnd::<TestStruct>::parse(&mut stream);
    // ...
}
```

...or implement it manually like this:

```rust
use parsable::*;

struct TestStruct {
    a: CharLiteral<b'a'>,
    b: CharLiteral<b'b'>,
}

impl<'a> Parsable<'a> for TestStruct {
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self>
    where
        Self: Sized
    {
        stream.scope(|stream| {
            Some(Ok(TestStruct {
                a: ok_or_throw!(CharLiteral::<b'a'>::parse(stream)?),
                b: ok_or_throw!(CharLiteral::<b'b'>::parse_or_error(stream)),
            }))
        })
    }

    fn error() -> ParseError {
        String::from("TestStruct")
    }
}

fn main(source: &[u8]) {
    let mut stream = ScopedStream::new(source);
    let outcome = WithEnd::<TestStruct>::parse(&mut stream);
    // ...
}
```

Use the `parsable_debug` flag to print debug messages when parsing using the types provided in this crate.

Additionally, the `parsable_derive_debug` config flag can be used to add debug messages to derived `Parsable` implementations.

### Included types

This library includes `Parsable` implementations for some common types, as well as generic types for easier usage.

| Name                                       | Description                                                                             |
|--------------------------------------------|-----------------------------------------------------------------------------------------|
| `Box<T>`                                   | Matches one instance of T and captures it into a `Box<T>`                               |
| `Option<T>`                                | Matches zero or one instance of T                                                       |
| `()`                                       | Matches zero tokens                                                                     |
| `CharLiteral<CHAR: u8>`                    | Matches one instance of the character CHAR                                              |
| `CharRange<START: u8, END: u8>`            | Matches one instance of any character in the range START to END (inclusive)             |
| `Repeat<T, MIN: usize>`                    | Matches a minimum of MIN repetitions of T                                               |
| `ZeroPlus<T> = Repeat<T, 0>`               | Matches zero or more repetitions of T                                                   |
| `OnePlus<T> = Repeat<T, 1>`                | Matches one or more repetitions of T                                                    |
| `RepeatLimited<T, MIN: usize, MAX: usize>` | Matches between MIN and MAX repetitions of T (inclusive)                                |
| `Intersperse<T, S>`                        | Matches any number of instances of T separated by single instances of S                 |
| `Ignore<T>`                                | Matches one instance of T without capturing the struct T                                |
| `WithSpan<T>`                              | Matches one instance of T and captures a span of the matched tokes                      |
| `Span<T>`                                  | Matches one instance of T and captures only the span of the matched range, discarding T |
| `WithIndex<T>`                             | Matches one instance of T and the starting index of the match                           |
| `EndOfStream`                              | Matches the end of the token stream                                                     |
| `WithEnd<T>`                               | Matches one instance of T followed by the end of the token stream                       |


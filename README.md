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

Example debugging commands (powershell):

`clear; $env:RUSTFLAGS='--cfg parsable_derive_debug'; cargo run --features parsable/parsable_debug -- examples/expr1.txt examples/out.ron; $env:RUSTFLAGS=''`

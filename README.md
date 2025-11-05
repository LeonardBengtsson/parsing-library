# parsable

## Usage

Use the `parsable_debug` flag to print debug messages when parsing using the types provided in this crate.

Additionally, the `parsable_derive_debug` config flag can be used to add debug messages to derived `Parsable` implementations.

Example debugging commands (powershell):

`clear; $env:RUSTFLAGS='--cfg parsable_derive_debug'; cargo run --features parsable/parsable_debug -- examples/expr1.txt examples/out.ron; $env:RUSTFLAGS=''`

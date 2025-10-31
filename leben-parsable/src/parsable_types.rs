use std::{fmt::Debug, marker::PhantomData};
use serde::{Deserialize, Serialize};

use crate::{Parsable, ParseError, ParseOutcome, ParseResult, ok_or_throw, ScopedStream};

#[derive(Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct WithSpan<T>
where
    T: for<'a> Parsable<'a>
{
    pub node: T,
    pub span: Vec<u8>,
}

impl<T> Debug for WithSpan<T>
where
    T: for<'a> Parsable<'a> + Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WithSpan")
            .field("node", &self.node)
            .field("span", &String::from_utf8_lossy(&self.span))
            .finish()
    }
}

impl<'a, T> Parsable<'a> for WithSpan<T>
where
    T: for<'b> Parsable<'b> + Debug
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<WithSpan<T>>
    {
        let res = stream
            .scope_with_span(|stream| T::parse(stream))
            .map(|result| result.map(
                |(node, span)| WithSpan { node, span: span.to_owned() }));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG SPAN        {:?}",
                res.as_ref().map(|span| String::from_utf8_lossy(&span.span)));
        }
        res
    }
    
    fn error() -> ParseError {
        T::error()
    }
    
    fn propagated_error() -> Option<ParseError> {
        None
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct Repeat<T, const MIN: usize>
where
    T: for<'a> Parsable<'a> + Debug
{
    pub nodes: Vec<T>,
}

impl<'a, T, const MIN: usize> Parsable<'a> for Repeat<T, MIN>
where 
    T: for<'b> Parsable<'b> + Debug
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG REPEAT >>>> {} * {}", std::any::type_name::<T>(), MIN);
        }
        let mut nodes = Vec::new();
        while let Some(node) = stream
            .scope(|stream| T::parse(stream))
        {
            nodes.push(ok_or_throw!(node));
        }
        let res = (nodes.len() >= MIN).then_some(Ok(Repeat { nodes }));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG REPEAT <<<< {} * {}\n{:?}", std::any::type_name::<T>(), MIN, &res);
        }
        res
    }

    fn error() -> ParseError {
        format!("a minimum of {} repetitions of {}", MIN, T::error())
    }
}

pub type ZeroPlus<T> = Repeat<T, 0>;

pub type OnePlus<T> = Repeat<T, 1>;

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct RepeatLimited<T, const MIN: usize, const MAX: usize>
where
    T: for<'a> Parsable<'a>
{
    pub nodes: Vec<T>,
}

impl<'a, T, const MIN: usize, const MAX: usize> Parsable<'a> for RepeatLimited<T, MIN, MAX>
where 
    T: for<'b> Parsable<'b> + Debug
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG REPEATLIM > {} * ({}-{})", std::any::type_name::<T>(), MIN, MAX);
        }
        let mut nodes = Vec::with_capacity(MAX);
        for i in 0..MAX {
            let node = stream
                .scope(|stream| T::parse(stream));

            if let Some(node) = node {
                nodes.push(ok_or_throw!(node));
            } else {
                let res = (i >= MIN).then_some(Ok(RepeatLimited { nodes }));
                #[cfg(feature = "leben_parsable_debug")] {
                    println!("DEBUG REPEATLIM < {} * ({}-{})\n{:?}", std::any::type_name::<T>(), MIN, MAX, &res);
                }
                return res;
            }
        }
        let res = Some(Ok(RepeatLimited { nodes }));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG REPEATLIM < {} * ({}-{})\n{:?}", std::any::type_name::<T>(), MIN, MAX, &res);
        }
        res
    }

    fn error() -> ParseError {
        format!("between {} and {} repetitions of {}", MIN, MAX, T::error())
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct CharLiteral<const CHAR: u8>;

impl<'a, const CHAR: u8> Parsable<'a> for CharLiteral<CHAR> {
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        stream.scope(|stream| {
            stream.read(1, |slice| slice[0] == CHAR)
                .map(|_| Ok(CharLiteral))
        })
    }
    
    fn error() -> ParseError {
        format!("'{}'", CHAR as char)
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct CharRange<const START: u8, const END: u8>;

impl<'a, const START: u8, const END: u8> Parsable<'a> for CharRange<START, END> {
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        stream.scope(|stream| {
            stream.read(1, |slice| (START..=END).contains(&slice[0]))
                .map(|_| Ok(CharRange))
        })
    }
    
    fn error() -> ParseError {
        format!("a character in the range '{}' to '{}'", START as char, END as char)
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct EndOfStream;

impl<'a> Parsable<'a> for EndOfStream {
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        let res = stream.at_end().then_some(Ok(EndOfStream));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG END         {:?}", &res);
        }
        res
    }
    
    fn error() -> ParseError {
        String::from("End of Stream")
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct WithEnd<T>
where
    T: for<'a> Parsable<'a>
{
    pub node: T
}

impl<'a, T> Parsable<'a> for WithEnd<T>
where
    T: for<'b> Parsable<'b>
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        let res = stream
            .scope(|stream| T::parse(stream))
            .map(|result| result.map(
                |node| WithEnd { node }));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG WITH END  < {}\n{:?}", std::any::type_name::<T>(), &res);
        }
        let res = ok_or_throw!(res?);
        ok_or_throw!(EndOfStream::parse_or_error(stream));
        Some(Ok(res))
    }
    
    fn error() -> ParseError {
        T::error()
    }
    
    fn propagated_error() -> Option<ParseError> {
        None
    }
}

#[derive(Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct Intersperse<T, S>
where
    T: for<'a> Parsable<'a>,
    S: for<'b> Parsable<'b>,
{
    pub nodes: Vec<T>,
    #[serde(skip_serializing)]
    phantom_data: PhantomData<S>,
}

impl<'a, T, S> Parsable<'a> for Intersperse<T, S>
where
    T: for<'t> Parsable<'t>,
    S: for<'s> Parsable<'s>,
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG INTER >>>>> {}", std::any::type_name::<T>());
        }
        let mut nodes = Vec::new();
        while let Some(node) = stream
            .scope(|stream| T::parse(stream))
        {
            let sep = stream.scope(|stream| S::parse(stream));
            if let Some(sep_result) = sep {
                ok_or_throw!(sep_result);
            } else {
                break;
            }
            nodes.push(ok_or_throw!(node));
        }
        let res = Some(Ok(Intersperse { nodes, phantom_data: PhantomData }));
        #[cfg(feature = "leben_parsable_debug")] {
            println!("DEBUG INTER <<<<< {}\n{:?}", std::any::type_name::<T>(), &res);
        }
        res
    }
    
    fn error() -> ParseError {
        format!("{} interspersed with {}", T::error(), S::error())
    }
}

impl<T, S> Debug for Intersperse<T, S>
where
    T: for<'t> Parsable<'t> + Debug,
    S: for<'s> Parsable<'s>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Intersperse").field("nodes", &self.nodes).finish()
    }
}

#[derive(Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct Ignore<T>
where
    T: for<'a> Parsable<'a> 
{
    #[serde(skip_serializing)]
    phantom_data: PhantomData<T>,
}

impl<T> Debug for Ignore<T>
where
    T: for<'a> Parsable<'a> 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ignore").finish()
    }
}

impl<'a, T> Parsable<'a> for Ignore<T>
where
    T: for<'b> Parsable<'b>
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        stream
            .scope(|stream| T::parse(stream))
            .map(|result| result.map(
                |_| Ignore { phantom_data: PhantomData }))
    }
    
    fn error() -> ParseError {
        T::error()
    }
    
    fn propagated_error() -> Option<ParseError> {
        None
    }
}

pub type Span<T> = WithSpan<Ignore<T>>;

#[derive(Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct WithIndex<T>
where
    T: for<'b> Parsable<'b>
{
    pub node: T,
    pub index: usize,
}

impl<'a, T> Parsable<'a> for WithIndex<T>
where
    T: for<'b> Parsable<'b> + Debug
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self>
    {
        let index = stream.index();
        stream
            .scope(|stream| T::parse(stream))
            .map(|result| result.map(
                |node| WithIndex { node, index }))
    }
    
    fn error() -> ParseError {
        T::error()
    }
    
    fn propagated_error() -> Option<ParseError> {
        None
    }
}

impl<'a, T> Parsable<'a> for Box<T>
where
    T: for<'b> Parsable<'b>
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        stream
            .scope(|stream| T::parse(stream))
            .map(|result| result.map(
                |node| Box::new(node)))
    }
    
    fn error() -> ParseError {
        T::error()
    }
    
    fn propagated_error() -> Option<ParseError> {
        None
    }
}

impl<'a, T> Parsable<'a> for Option<T>
where 
    T: for<'b> Parsable<'b>
{
    fn parse(stream: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        stream
            .scope(|stream| T::parse(stream))
            .map(|result| result.map(
                |node| Some(node)))
            .or(Some(Ok(None)))
    }
    
    fn error() -> ParseError {
        // unreachable under normal circumstances
        format!("optional {}", T::error())
    }
}

impl<'a> Parsable<'a> for () {
    fn parse(_: &mut ScopedStream<'a>) -> ParseOutcome<Self> {
        Some(Ok(()))
    }
    
    fn error() -> ParseError {
        // unreachable under normal circumstances
        format!("empty token")
    }
}

pub fn parse_literal<'a>(stream: &mut ScopedStream<'a>, literal: &'static [u8]) -> ParseOutcome<()> {
    let res = stream.scope(|stream| {
        stream.read(literal.len(), |slice| slice == literal)
            .map(|_| Ok(()))
    });
    #[cfg(feature = "leben_parsable_debug")] {
        let lit = String::from_utf8_lossy(literal);
        println!("DEBUG LITERAL     {} {:?}", &lit, res.as_ref().map(|_| &lit));
    }
    res
}

pub fn parse_literal_or_error<'a>(stream: &mut ScopedStream<'a>, literal: &'static [u8]) -> ParseResult<()> {
    parse_literal(stream, literal)
        .unwrap_or_else(|| Err(vec![stream.error_literal_value_here(literal)]))
}

#[macro_export]
macro_rules! literals {
    { $( $vis:vis struct $name:ident = $lit:literal; )* } => {
        $(
            #[derive(Parsable, Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
            $vis struct $name {
                #[literal = $lit] _0: (),
            }

            impl std::fmt::Debug for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.debug_tuple(stringify!($name)).field(&stringify!($lit)).finish()
                }
            }
        )*
    }
}

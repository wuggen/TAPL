use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;

mod combinators;

pub use combinators::*;

pub trait Parser<T> {
    type Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>;

    fn in_first(&self, input: &Self::Input) -> bool;

    fn empty_allowed(&self) -> bool;

    fn map<F>(self, f: F) -> Map<T, Self, F>
    where
        Self: Sized,
    {
        Map(self, f, PhantomData)
    }

    fn require(self) -> Require<Self>
    where
        Self: Sized,
    {
        Require(self)
    }

    fn repeat(self, n: usize) -> Repeat<Self>
    where
        Self: Sized,
    {
        Repeat(self, n)
    }

    fn alt<P>(self, other: P) -> Alt<Self, P>
    where
        Self: Sized,
    {
        Alt(self, other)
    }
}

impl<P, T> Parser<T> for &'_ mut P
where
    P: Parser<T>,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        (*self).parse(input)
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        (**self).in_first(input)
    }

    fn empty_allowed(&self) -> bool {
        (**self).empty_allowed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Map<T, P, F>(P, F, PhantomData<T>);

impl<P, F, T, S> Parser<S> for Map<T, P, F>
where
    P: Parser<T>,
    F: Fn(T) -> S,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<S>>
    where
        I: Iterator<Item = Self::Input>,
    {
        self.0.parse(input).map(|o| o.map(|t| (self.1)(t)))
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        self.0.in_first(input)
    }

    fn empty_allowed(&self) -> bool {
        self.0.empty_allowed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Require<P>(P);

impl<P, T> Parser<T> for Require<P>
where
    P: Parser<T>,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        Some(
            self.0
                .parse(input)
                .ok_or_else(|| Error::from(ErrorKind::UnexpectedEndOfInput))
                .flatten(),
        )
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        self.0.in_first(input)
    }

    fn empty_allowed(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Repeat<P>(P, usize);

impl<P, T> Parser<Vec<T>> for Repeat<P>
where
    P: Parser<T>,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<Vec<T>>>
    where
        I: Iterator<Item = Self::Input>,
    {
        let mut res = Vec::with_capacity(self.1);
        for _ in 0..self.1 {
            match (&mut self.0).require().parse(input) {
                None => return Some(Err(Error::from(ErrorKind::UnexpectedEndOfInput))),
                Some(Err(e)) => return Some(Err(e)),
                Some(Ok(t)) => res.push(t),
            }
        }

        Some(Ok(res))
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        self.0.in_first(input)
    }

    fn empty_allowed(&self) -> bool {
        self.1 == 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Alt<P, R>(P, R);

impl<P, R, T> Parser<T> for Alt<P, R>
where
    P: Parser<T>,
    R: Parser<T, Input = P::Input>,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        if let Some(tok) = input.next() {
            if self.0.in_first(&tok) {
                self.0.parse(&mut std::iter::once(tok).chain(input))
            } else {
                self.1.parse(&mut std::iter::once(tok).chain(input))
            }
        } else {
            if self.empty_allowed() {
                None
            } else {
                Some(Err(Error::from(ErrorKind::UnexpectedEndOfInput)))
            }
        }
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        self.0.in_first(input) || self.1.in_first(input)
    }

    fn empty_allowed(&self) -> bool {
        self.0.empty_allowed() || self.1.empty_allowed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct AndThen<T, P, R>(P, R, PhantomData<T>);

impl<P, R, T, S> Parser<S> for AndThen<T, P, R>
where
    P: Parser<T>,
    R: Parser<S, Input = P::Input>,
{
    type Input = P::Input;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<S>>
    where
        I: Iterator<Item = Self::Input>,
    {
        todo!()
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        todo!()
    }

    fn empty_allowed(&self) -> bool {
        todo!()
    }
}

//#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
//pub struct Seq<P, R>(P, R);
//
//impl<P, R, T, S> Parser<(T, S)> for Seq<P, R>
//where
//    P: Parser<T>,
//    R: Parser<S, Input = P::Input>,
//{
//    type Input = P::Input;
//
//    fn parse<I>(&mut self, input: &mut I) -> Option<Result<(T, S)>>
//    where
//        I: Iterator<Item = Self::Input>,
//    {
//
//    }
//
//    fn in_first(&self, input: &Self::Input) -> bool {
//        todo!()
//    }
//
//    fn empty_allowed(&self) -> bool {
//        todo!()
//    }
//}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
pub enum ErrorKind {
    #[error("unexpected token in input stream")]
    UnexpectedToken,

    #[error("unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("trailing input")]
    TrailingInput,

    #[error("unspecified error")]
    Other,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    source: Option<Box<dyn std::error::Error + 'static>>,
}

impl Error {
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn new<E>(kind: ErrorKind, source: E) -> Self
    where
        E: std::error::Error + 'static,
    {
        Self {
            kind,
            source: Some(Box::new(source)),
        }
    }

    pub fn other<E>(source: E) -> Self
    where
        E: std::error::Error + 'static,
    {
        Self {
            kind: ErrorKind::Other,
            source: Some(Box::new(source)),
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self { kind, source: None }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)?;

        if let Some(source) = std::error::Error::source(self) {
            write!(f, ": {}", source)?;
        }

        Ok(())
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_deref()
    }
}

pub type Result<T> = std::result::Result<T, Error>;

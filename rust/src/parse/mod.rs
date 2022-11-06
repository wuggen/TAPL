use std::borrow::Cow;
use std::fmt::{self, Write};
use std::iter::Peekable;

pub use internal::SymbolSet;

mod internal;

pub trait Parser {
    type Output;

    fn start_set(&self) -> SymbolSet;

    fn accepts_empty(&self) -> bool;

    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result;

    fn expected_string(&self) -> String {
        let mut s = String::new();
        self.expected(&mut s).unwrap();
        s
    }

    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
    where
        I: Iterator<Item = char>;

    fn repeat(self) -> Repeat<Self>
    where
        Self: Sized,
    {
        Repeat(self)
    }

    fn alternate<P>(self, other: P) -> Alternate<Self, P>
    where
        Self: Sized,
    {
        Alternate(self, other)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected symbol {:?} in input (note: expected {})", sym, expected)]
    UnexpectedSymbol { sym: char, expected: String },

    #[error("unexpected end of input (note: expected {})", expected)]
    UnexpectedEndOfInput { expected: String },

    #[error(transparent)]
    Other {
        source: Box<dyn std::error::Error + 'static>,
    },

    #[error("unspecified parsing error")]
    Unspecified,
}

impl Error {
    fn replace_message(self, message: impl Into<String>) -> Self {
        self.map_message(|_| message.into())
    }

    fn map_message(self, f: impl FnOnce(String) -> String) -> Self {
        match self {
            Self::UnexpectedSymbol { sym, expected } => Self::UnexpectedSymbol {
                sym,
                expected: f(expected),
            },
            Self::UnexpectedEndOfInput { expected } => Self::UnexpectedEndOfInput {
                expected: f(expected),
            },
            e => e,
        }
    }

    fn unexpected_symbol(sym: char, expected: impl Into<String>) -> Self {
        Self::UnexpectedSymbol {
            sym,
            expected: expected.into(),
        }
    }

    fn eof(expected: impl Into<String>) -> Self {
        Self::UnexpectedEndOfInput {
            expected: expected.into(),
        }
    }

    pub fn other<E: std::error::Error + 'static>(source: E) -> Self {
        Self::Other {
            source: Box::new(source),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Singleton(char);

pub fn singleton(c: char) -> Singleton {
    Singleton(c)
}

impl Parser for Singleton {
    type Output = Option<char>;

    fn start_set(&self) -> SymbolSet {
        SymbolSet::singleton(self.0)
    }

    fn accepts_empty(&self) -> bool {
        true
    }

    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
        write!(out, "character {:?}", self.0)
    }

    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
    where
        I: Iterator<Item = char>,
    {
        if input.peek() == Some(&self.0) {
            input.next();
            Ok(Some(self.0))
        } else {
            Ok(None)
        }
    }
}

pub struct Range(char, char);

pub fn range(start: char, end: char) -> Range {
    assert!(start <= end, "empty range");
    Range(start, end)
}

impl Parser for Range {
    type Output = Option<char>;

    fn start_set(&self) -> SymbolSet {
        SymbolSet::range(self.0, self.1)
    }

    fn accepts_empty(&self) -> bool {
        true
    }

    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
        write!(out, "character in the range {:?} to {:?}", self.0, self.1)
    }

    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
    where
        I: Iterator<Item = char>,
    {
        if let Some(c) = input.peek() {
            if *c >= self.0 && *c <= self.1 {
                Ok(Some(*c))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

pub struct Sequence<'a>(Cow<'a, str>);

pub fn str_slice(s: &str) -> Sequence {
    assert!(!s.is_empty(), "empty sequence");
    Sequence(Cow::Borrowed(s))
}

pub fn string(s: String) -> Sequence<'static> {
    assert!(!s.is_empty(), "empty sequence");
    Sequence(Cow::Owned(s))
}

impl Parser for Sequence<'_> {
    type Output = Option<String>;

    fn start_set(&self) -> SymbolSet {
        SymbolSet::singleton(self.0.chars().next().unwrap())
    }

    fn accepts_empty(&self) -> bool {
        true
    }

    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
        write!(out, "sequence {:?}", self.0)
    }

    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
    where
        I: Iterator<Item = char>,
    {
        if let Some(c) = input.peek() {
            if *c == self.0.chars().next().unwrap() {
                for expected in self.0.chars() {
                    if let Some(c) = input.peek() {
                        if *c == expected {
                            input.next();
                        } else {
                            return Err(Error::unexpected_symbol(*c, self.expected_string()));
                        }
                    } else {
                        return Err(Error::eof(self.expected_string()));
                    }
                }

                Ok(Some(String::from(self.0.as_ref())))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

pub struct Repeat<P>(P);

impl<T, P: Parser<Output = Option<T>>> Parser for Repeat<P> {
    type Output = Vec<T>;

    fn start_set(&self) -> SymbolSet {
        self.0.start_set()
    }

    fn accepts_empty(&self) -> bool {
        true
    }

    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
        write!(out, "sequence of ")?;
        self.0.expected(out)
    }

    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
    where
        I: Iterator<Item = char>,
    {
        let mut output = Vec::new();

        while let Some(item) = self
            .0
            .parse(input)
            .map_err(|e| e.replace_message(self.expected_string()))?
        {
            output.push(item);
        }

        Ok(output)
    }
}

pub struct Alternate<P, R>(P, R);

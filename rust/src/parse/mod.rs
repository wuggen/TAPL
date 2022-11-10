//use std::borrow::Cow;
//use std::fmt::{self, Write};
//use std::iter::Peekable;
//
//pub use internal::SymbolSet;

use std::collections::VecDeque;
use std::ops::RangeInclusive;

mod internal;

pub struct ParserInput<T> {
    input: Box<dyn Iterator<Item = T>>,
    lookahead: VecDeque<T>,
}

impl<T> ParserInput<T> {
    pub fn new<I>(input: I) -> Self
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: 'static,
    {
        let input = Box::new(input.into_iter().fuse());
        let lookahead = VecDeque::new();
        Self { input, lookahead }
    }

    /// Look ahead `n` symbols in the input.
    ///
    /// This does not advance the input. Subsequent calls to `lookahead_n` without intervening
    /// advances will contain the returned slice as as a prefix (if called with greater `n`) or
    /// will be prefixes of the returned slice (if called with lesser `n`).
    ///
    /// Returns a slice of length at most `n`. The length will be shorter if the remaining input is
    /// shorter than `n`.
    pub fn lookahead_n(&mut self, n: usize) -> &[T] {
        if self.lookahead.len() < n {
            self.lookahead_additional(n - self.lookahead.len());
        }

        let actual = usize::min(n, self.lookahead.len());
        &self.lookahead.make_contiguous()[0..actual]
    }

    /// Advance the input `n` symbols.
    ///
    /// Returns the symbols advanced over. This may be less than `n` if the remaining input is
    /// shorter than `n`.
    pub fn advance_n(&mut self, n: usize) -> Vec<T> {
        let already_buffered = usize::min(n, self.lookahead.len());
        let additional_needed = n - already_buffered;

        let mut syms: Vec<T> = self.lookahead.drain(0..already_buffered).collect();

        for _ in 0..additional_needed {
            if let Some(sym) = self.input.next() {
                syms.push(sym);
            } else {
                break;
            }
        }

        syms
    }

    /// Look ahead at the next symbol in the input.
    ///
    /// This does not advance the input. Subsequent calls to `lookahead` without intervening
    /// advances will return the same symbol.
    pub fn lookahead(&mut self) -> Option<&T> {
        if self.lookahead.is_empty() {
            self.lookahead_additional(1);
        }
        self.lookahead.get(0)
    }

    /// Advance the input one symbol.
    pub fn advance(&mut self) -> Option<T> {
        if self.lookahead.is_empty() {
            self.input.next()
        } else {
            self.lookahead.pop_front()
        }
    }
}

impl<T> ParserInput<T>
where
    T: Eq,
{
    /// Attempts to advance over one symbol equal to an expected symbol.
    ///
    /// If the input is empty, or if the next symbol is not equal to the expected symbol, returns
    /// `None` and does not advance the input. Otherwise, advances the input by one symbol and
    /// returns the extracted symbol.
    pub fn one(&mut self, expected: &T) -> Option<T> {
        if let Some(t) = self.lookahead() {
            if t == expected {
                self.advance()
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Attempts to advance over as many symbols equal to the expected symbol as possible.
    ///
    /// Advances the input by as many symbols equal to `expected` are encountered. Returns a vec of
    /// all extracted symbols.
    pub fn many(&mut self, expected: &T) -> Vec<T> {
        let mut syms = Vec::new();
        while let Some(t) = self.lookahead() {
            if t == expected {
                syms.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        syms
    }
}

impl<T> ParserInput<T>
where
    T: Ord,
{
    /// Attempts to advance over one symbol in the given range.
    ///
    /// If the input is empty, or if the next symbol is not within the given range, returns `None`
    /// and does not advance the input. Otherwise, advances the input by one symbol and returns the
    /// extracted symbol.
    pub fn one_in_range(&mut self, expected: RangeInclusive<T>) -> Option<T> {
        if let Some(t) = self.lookahead() {
            todo!()
        } else {
            None
        }
    }
}

impl<T> ParserInput<T> {
    /// Advance the input by `amount` items, appending them to the lookahead buffer.
    ///
    /// Returns the number of items actually appended. This will be less than `amount` if the input
    /// contains fewer than `amount` additional symbols.
    fn lookahead_additional(&mut self, amount: usize) -> usize {
        for n in 0..amount {
            if let Some(item) = self.input.next() {
                self.lookahead.push_back(item);
            } else {
                return n;
            }
        }

        amount
    }
}

//pub trait Parser {
//    type Output;
//
//    fn start_set(&self) -> SymbolSet;
//
//    fn accepts_empty(&self) -> bool;
//
//    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result;
//
//    fn expected_string(&self) -> String {
//        let mut s = String::new();
//        self.expected(&mut s).unwrap();
//        s
//    }
//
//    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
//    where
//        I: Iterator<Item = char>;
//
//    fn repeat(self) -> Repeat<Self>
//    where
//        Self: Sized,
//    {
//        Repeat(self)
//    }
//
//    fn alternate<P>(self, other: P) -> Alternate<Self, P>
//    where
//        Self: Sized,
//    {
//        Alternate(self, other)
//    }
//}
//
//#[derive(Debug, thiserror::Error)]
//pub enum Error {
//    #[error("unexpected symbol {:?} in input (note: expected {})", sym, expected)]
//    UnexpectedSymbol { sym: char, expected: String },
//
//    #[error("unexpected end of input (note: expected {})", expected)]
//    UnexpectedEndOfInput { expected: String },
//
//    #[error(transparent)]
//    Other {
//        source: Box<dyn std::error::Error + 'static>,
//    },
//
//    #[error("unspecified parsing error")]
//    Unspecified,
//}
//
//impl Error {
//    fn replace_message(self, message: impl Into<String>) -> Self {
//        self.map_message(|_| message.into())
//    }
//
//    fn map_message(self, f: impl FnOnce(String) -> String) -> Self {
//        match self {
//            Self::UnexpectedSymbol { sym, expected } => Self::UnexpectedSymbol {
//                sym,
//                expected: f(expected),
//            },
//            Self::UnexpectedEndOfInput { expected } => Self::UnexpectedEndOfInput {
//                expected: f(expected),
//            },
//            e => e,
//        }
//    }
//
//    fn unexpected_symbol(sym: char, expected: impl Into<String>) -> Self {
//        Self::UnexpectedSymbol {
//            sym,
//            expected: expected.into(),
//        }
//    }
//
//    fn eof(expected: impl Into<String>) -> Self {
//        Self::UnexpectedEndOfInput {
//            expected: expected.into(),
//        }
//    }
//
//    pub fn other<E: std::error::Error + 'static>(source: E) -> Self {
//        Self::Other {
//            source: Box::new(source),
//        }
//    }
//}
//
//pub type Result<T> = std::result::Result<T, Error>;
//
//pub struct Singleton(char);
//
//pub fn singleton(c: char) -> Singleton {
//    Singleton(c)
//}
//
//impl Parser for Singleton {
//    type Output = Option<char>;
//
//    fn start_set(&self) -> SymbolSet {
//        SymbolSet::singleton(self.0)
//    }
//
//    fn accepts_empty(&self) -> bool {
//        true
//    }
//
//    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
//        write!(out, "character {:?}", self.0)
//    }
//
//    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
//    where
//        I: Iterator<Item = char>,
//    {
//        if input.peek() == Some(&self.0) {
//            input.next();
//            Ok(Some(self.0))
//        } else {
//            Ok(None)
//        }
//    }
//}
//
//pub struct Range(char, char);
//
//pub fn range(start: char, end: char) -> Range {
//    assert!(start <= end, "empty range");
//    Range(start, end)
//}
//
//impl Parser for Range {
//    type Output = Option<char>;
//
//    fn start_set(&self) -> SymbolSet {
//        SymbolSet::range(self.0, self.1)
//    }
//
//    fn accepts_empty(&self) -> bool {
//        true
//    }
//
//    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
//        write!(out, "character in the range {:?} to {:?}", self.0, self.1)
//    }
//
//    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
//    where
//        I: Iterator<Item = char>,
//    {
//        if let Some(c) = input.peek() {
//            if *c >= self.0 && *c <= self.1 {
//                Ok(Some(*c))
//            } else {
//                Ok(None)
//            }
//        } else {
//            Ok(None)
//        }
//    }
//}
//
//pub struct Sequence<'a>(Cow<'a, str>);
//
//pub fn str_slice(s: &str) -> Sequence {
//    assert!(!s.is_empty(), "empty sequence");
//    Sequence(Cow::Borrowed(s))
//}
//
//pub fn string(s: String) -> Sequence<'static> {
//    assert!(!s.is_empty(), "empty sequence");
//    Sequence(Cow::Owned(s))
//}
//
//impl Parser for Sequence<'_> {
//    type Output = Option<String>;
//
//    fn start_set(&self) -> SymbolSet {
//        SymbolSet::singleton(self.0.chars().next().unwrap())
//    }
//
//    fn accepts_empty(&self) -> bool {
//        true
//    }
//
//    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
//        write!(out, "sequence {:?}", self.0)
//    }
//
//    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
//    where
//        I: Iterator<Item = char>,
//    {
//        if let Some(c) = input.peek() {
//            if *c == self.0.chars().next().unwrap() {
//                for expected in self.0.chars() {
//                    if let Some(c) = input.peek() {
//                        if *c == expected {
//                            input.next();
//                        } else {
//                            return Err(Error::unexpected_symbol(*c, self.expected_string()));
//                        }
//                    } else {
//                        return Err(Error::eof(self.expected_string()));
//                    }
//                }
//
//                Ok(Some(String::from(self.0.as_ref())))
//            } else {
//                Ok(None)
//            }
//        } else {
//            Ok(None)
//        }
//    }
//}
//
//pub struct Repeat<P>(P);
//
//impl<T, P: Parser<Output = Option<T>>> Parser for Repeat<P> {
//    type Output = Vec<T>;
//
//    fn start_set(&self) -> SymbolSet {
//        self.0.start_set()
//    }
//
//    fn accepts_empty(&self) -> bool {
//        true
//    }
//
//    fn expected<W: Write>(&self, out: &mut W) -> fmt::Result {
//        write!(out, "sequence of ")?;
//        self.0.expected(out)
//    }
//
//    fn parse<I>(&self, input: &mut Peekable<I>) -> Result<Self::Output>
//    where
//        I: Iterator<Item = char>,
//    {
//        let mut output = Vec::new();
//
//        while let Some(item) = self
//            .0
//            .parse(input)
//            .map_err(|e| e.replace_message(self.expected_string()))?
//        {
//            output.push(item);
//        }
//
//        Ok(output)
//    }
//}
//
//pub struct Alternate<P, R>(P, R);

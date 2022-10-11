use std::borrow::Cow;
use std::fmt::Debug;
use std::marker::PhantomData;

use super::*;

pub fn one<T>(c: T) -> One<T> {
    One(c)
}

pub fn any<T>() -> Any<T> {
    Any(PhantomData)
}

pub fn subseq<T: Clone>(seq: &[T]) -> Subseq<T> {
    Subseq::from(seq)
}

pub fn owned_subseq<T: Clone>(seq: Vec<T>) -> OwnedSubseq<T> {
    Subseq::from(seq)
}

pub fn substr(s: &str) -> Substr {
    Substr::from(s)
}

pub fn owned_substr(s: String) -> OwnedSubstr {
    Substr::from(s)
}

pub fn eof<T>() -> Eof<T> {
    Eof(PhantomData)
}

pub fn predicate<F, D: Display + ?Sized>(pred: F, description: &D) -> Predicate<F> {
    Predicate(pred, description.to_string())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct One<T>(T);

impl<T> Parser<T> for One<T>
where
    T: Clone + Eq + Debug + 'static,
{
    type Input = T;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        input.next().map(|c| {
            if c == self.0 {
                Ok(c)
            } else {
                Err(CombinatorError::unexpected(self.0.clone(), c))
            }
        })
    }

    fn in_first(&self, input: &T) -> bool {
        input == &self.0
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Any<T>(PhantomData<T>);

impl<T> Parser<T> for Any<T> {
    type Input = T;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        input.next().map(Ok)
    }

    fn in_first(&self, _input: &T) -> bool {
        true
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Subseq<'a, T: Clone>(Cow<'a, [T]>);
pub type OwnedSubseq<T> = Subseq<'static, T>;

impl<T: Clone> From<Vec<T>> for Subseq<'static, T> {
    fn from(seq: Vec<T>) -> Self {
        assert!(!seq.is_empty());
        Self(Cow::Owned(seq))
    }
}

impl<'a, T: Clone> From<&'a [T]> for Subseq<'a, T> {
    fn from(seq: &'a [T]) -> Self {
        assert!(!seq.is_empty());
        Self(Cow::Borrowed(seq))
    }
}

impl<T> Parser<Vec<T>> for Subseq<'_, T>
where
    T: Debug + Clone + Eq + 'static,
{
    type Input = T;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<Vec<T>>>
    where
        I: Iterator<Item = Self::Input>,
    {
        let mut res = Vec::new();
        for (input, expected) in input.zip(self.0.iter()) {
            if &input == expected {
                res.push(input);
            } else {
                return Some(Err(CombinatorError::unexpected(expected.clone(), input)));
            }
        }

        if res.is_empty() {
            None
        } else {
            Some(Ok(res))
        }
    }

    fn in_first(&self, input: &T) -> bool {
        input == &self.0[0]
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Substr<'a>(Cow<'a, str>);
pub type OwnedSubstr = Substr<'static>;

impl From<String> for OwnedSubstr {
    fn from(s: String) -> Self {
        Self(Cow::Owned(s))
    }
}

impl<'a> From<&'a str> for Substr<'a> {
    fn from(s: &'a str) -> Self {
        Self(Cow::Borrowed(s))
    }
}

impl Parser<String> for Substr<'_> {
    type Input = char;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<String>>
    where
        I: Iterator<Item = Self::Input>,
    {
        let mut res = String::new();
        for (input, expected) in input.zip(self.0.chars()) {
            if input == expected {
                res.push(input);
            } else {
                return Some(Err(CombinatorError::unexpected(expected, input)));
            }
        }

        if res.is_empty() {
            None
        } else {
            Some(Ok(res))
        }
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        *input == self.0.chars().next().unwrap()
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Eof<T>(PhantomData<T>);

impl<T: Debug + 'static> Parser<()> for Eof<T> {
    type Input = T;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<()>>
    where
        I: Iterator<Item = Self::Input>,
    {
        if let Some(received) = input.next() {
            Some(Err(CombinatorError::trailing(received)))
        } else {
            Some(Ok(()))
        }
    }

    fn in_first(&self, _input: &Self::Input) -> bool {
        false
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Predicate<F>(F, String);

impl<F, T> Parser<T> for Predicate<F>
where
    F: Fn(&T) -> bool,
    T: Debug + 'static,
{
    type Input = T;

    fn parse<I>(&mut self, input: &mut I) -> Option<Result<T>>
    where
        I: Iterator<Item = Self::Input>,
    {
        input.next().map(|c| {
            if (self.0)(&c) {
                Ok(c)
            } else {
                Err(CombinatorError::unexpected_msg(&self.1, c))
            }
        })
    }

    fn in_first(&self, input: &Self::Input) -> bool {
        (self.0)(input)
    }

    fn empty_allowed(&self) -> bool {
        true
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CombinatorError<C> {
    #[error("expected {expected}, read {received:?}")]
    UnexpectedToken { expected: Expected<C>, received: C },

    #[error("expected end of input, received {received:?}")]
    TrailingInput { received: C },
}

impl<C: Debug + 'static> CombinatorError<C> {
    pub fn unexpected(expected: C, received: C) -> Error {
        Error::new(
            ErrorKind::UnexpectedToken,
            Self::UnexpectedToken {
                expected: Expected::Token(expected),
                received,
            },
        )
    }

    pub fn unexpected_msg<D: Display + ?Sized>(expected: &D, received: C) -> Error {
        Error::new(
            ErrorKind::UnexpectedToken,
            Self::UnexpectedToken {
                expected: Expected::Description(expected.to_string()),
                received,
            },
        )
    }

    pub fn trailing(received: C) -> Error {
        Error::new(ErrorKind::TrailingInput, Self::TrailingInput { received })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected<C> {
    Token(C),
    Description(String),
}

impl<C: Debug> Display for Expected<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(c) => write!(f, "{:?}", c),
            Self::Description(s) => write!(f, "{}", s),
        }
    }
}

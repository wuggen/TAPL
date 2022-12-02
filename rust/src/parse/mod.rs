use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::FromStr;

pub mod prelude {
    pub use super::{
        extract_until, extract_while, identifier, integer, literal, one, predicate, whitespace,
        Error, IResult, Parser,
    };
}

/// Composable parsers.
pub trait Parser {
    /// The type parsed by this `Parser`.
    type Output<'i>;

    /// Attempt to parse a `Self::Output` from the given string.
    ///
    /// This returns an [`IResult`], a typedef for a `Result` that, when `Ok`, returns the
    /// remaining unconsumed portion of the input.
    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>>;

    fn ignore(self) -> Ignore<Self>
    where
        Self: Sized,
    {
        Ignore(self)
    }

    fn then<P: Parser>(self, other: P) -> Then<Self, P>
    where
        Self: Sized,
    {
        Then(self, other)
    }

    fn then_ignore<P: Parser>(self, other: P) -> ThenIgnore<Self, P>
    where
        Self: Sized,
    {
        ThenIgnore(self.then(other))
    }

    fn ignore_then<P: Parser>(self, other: P) -> IgnoreThen<Self, P>
    where
        Self: Sized,
    {
        IgnoreThen(self.then(other))
    }

    /// Construct a parser that will try this parser and then the `other`, returning whichever
    /// succeeds.
    fn alternate<P: for<'i> Parser<Output<'i> = Self::Output<'i>>>(
        self,
        other: P,
    ) -> Alternate<Self, P>
    where
        Self: Sized,
    {
        Alternate { p: self, q: other }
    }

    /// Construct a parser that will run this parser, then apply the given function to its output.
    fn map<F, T>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: for<'i> Fn(Self::Output<'i>) -> T,
    {
        Map { p: self, f }
    }

    fn try_map<F, T>(self, f: F) -> TryMap<Self, F>
    where
        Self: Sized,
        F: for<'i> Fn(Self::Output<'i>) -> Result<T>,
    {
        TryMap(self.map(f))
    }

    /// Construct a parser that will run this parser and, if it fails, apply the given function to
    /// the error value.
    fn map_error<F>(self, f: F) -> MapError<Self, F>
    where
        Self: Sized,
        F: Fn(Error) -> Error,
    {
        MapError { p: self, f }
    }

    /// Construct a parser that will attempt to run this parser exactly `n` times in sequence.
    fn repeat(self, n: usize) -> Repeat<Self>
    where
        Self: Sized,
    {
        Repeat { p: self, n }
    }

    /// Construct a parser that will run this parser sequentially as many times as it succeeds.
    ///
    /// This parser expects to succeed at least once. Follow up with [`Parser::optional`] to allow
    /// the empty sequence.
    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many(self)
    }

    /// Construct a parser tha will attempt to run this parser and, if it fails, return nothing.
    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self)
    }

    /// Construct a parser that will run this one and additionally return the consumed portion of
    /// the input.
    fn consumed(self) -> Consumed<Self>
    where
        Self: Sized,
    {
        Consumed(self)
    }

    /// Construct a parser that will run this one and then map a type's [`FromStr`] implementation
    /// over the consumed input.
    fn map_from_str<T>(self) -> MapFromStr<Self, T>
    where
        Self: Sized,
    {
        MapFromStr {
            p: self.consumed(),
            _t: PhantomData,
        }
    }

    /// Construct a parser that will run this one and fail if there is any remaining input.
    fn strict(self) -> Strict<Self>
    where
        Self: Sized,
    {
        Strict(self)
    }
}

pub struct Ignore<P>(P);

impl<P: Parser> Parser for Ignore<P> {
    type Output<'i> = ();

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (_, rem) = self.0.parse(input)?;
        Ok(((), rem))
    }
}

pub struct Then<P, Q>(P, Q);

impl<P: Parser, Q: Parser> Parser for Then<P, Q> {
    type Output<'i> = (P::Output<'i>, Q::Output<'i>);

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (p, rem) = self.0.parse(input)?;
        let (q, rem) = self.1.parse(rem)?;
        Ok(((p, q), rem))
    }
}

pub struct ThenIgnore<P, Q>(Then<P, Q>);

impl<P: Parser, Q: Parser> Parser for ThenIgnore<P, Q> {
    type Output<'i> = P::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let ((p, _), rem) = self.0.parse(input)?;
        Ok((p, rem))
    }
}

pub struct IgnoreThen<P, Q>(Then<P, Q>);

impl<P: Parser, Q: Parser> Parser for IgnoreThen<P, Q> {
    type Output<'i> = Q::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let ((_, q), rem) = self.0.parse(input)?;
        Ok((q, rem))
    }
}

/// A parser that alternates between two parsers.
///
/// Constructed via the [`Parser::alternate`] method.
pub struct Alternate<P, Q> {
    p: P,
    q: Q,
}

impl<P, Q> Parser for Alternate<P, Q>
where
    P: Parser,
    Q: for<'i> Parser<Output<'i> = P::Output<'i>>,
{
    type Output<'i> = P::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        self.p.parse(input).or_else(|_| self.q.parse(input))
    }
}

/// A parser that maps a function over its output.
///
/// Constructed via the [`Parser::map`] method.
pub struct Map<P, F> {
    p: P,
    f: F,
}

impl<P, F, T> Parser for Map<P, F>
where
    P: Parser,
    F: for<'i> Fn(P::Output<'i>) -> T,
{
    type Output<'i> = T;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (res, rem) = self.p.parse(input)?;
        Ok(((self.f)(res), rem))
    }
}

pub struct TryMap<P, F>(Map<P, F>);

impl<P, F, T> Parser for TryMap<P, F>
where
    P: Parser,
    F: for<'i> Fn(P::Output<'i>) -> Result<T>,
{
    type Output<'i> = T;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (res, rem) = self.0.parse(input)?;
        Ok((res?, rem))
    }
}

/// A parser that maps a type's [`FromStr`] implementation over the consumed input.
///
/// Constructed via the [`Parser::map_from_str`] method.
pub struct MapFromStr<P, T> {
    p: Consumed<P>,
    _t: PhantomData<T>,
}

impl<P, T> Parser for MapFromStr<P, T>
where
    P: Parser,
    T: FromStr,
    T::Err: std::error::Error + 'static,
{
    type Output<'i> = T;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (consumed, rem) = self.p.parse(input)?;
        let out = T::from_str(consumed).map_err(Error::custom)?;
        Ok((out, rem))
    }
}

/// A parser which applies a function to the error value if it fails.
///
/// Constructed via the [`Parser::map_error`] method.
pub struct MapError<P, F> {
    p: P,
    f: F,
}

impl<P, F> Parser for MapError<P, F>
where
    P: Parser,
    F: Fn(Error) -> Error,
{
    type Output<'i> = P::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        self.p.parse(input).map_err(&self.f)
    }
}

/// A parser that iterates a fixed number of times.
///
/// Constructed via the [`Parser::repeat`] method.
pub struct Repeat<P> {
    p: P,
    n: usize,
}

impl<P: Parser> Parser for Repeat<P> {
    type Output<'i> = Vec<P::Output<'i>>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let mut rem = input;
        let mut out = Vec::with_capacity(self.n);

        for _ in 0..self.n {
            let (item, r) = self.p.parse(rem)?;
            rem = r;
            out.push(item);
        }

        Ok((out, rem))
    }
}

/// A parser that iterates at least once, and as many times as possible.
///
/// Constructed via the [`Parser::many`] method.
pub struct Many<P>(P);

impl<P: Parser> Parser for Many<P>
where
    for<'i> P::Output<'i>: Debug,
{
    type Output<'i> = Vec<P::Output<'i>>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (first, mut rem) = self.0.parse(input)?;
        let mut out = vec![first];

        while let Ok((item, r)) = self.0.parse(rem) {
            rem = r;
            out.push(item);
        }

        Ok((out, rem))
    }
}

/// A parser that, if it fails, returns `None` instead of an error.
///
/// Constructed via the [`Parser::optional`] method.
pub struct Optional<P>(P);

impl<P: Parser> Parser for Optional<P> {
    type Output<'i> = Option<P::Output<'i>>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        self.0
            .parse(input)
            .map(|(out, rem)| (Some(out), rem))
            .or(Ok((None, input)))
    }
}

/// A parser that returns the portion of the input consumed.
///
/// Constructed via the [`Parser::consumed`] method.
pub struct Consumed<P>(P);

impl<P: Parser> Parser for Consumed<P> {
    type Output<'i> = &'i str;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (_, rem) = self.0.parse(input)?;
        let diff = input.len() - rem.len();
        let (consumed, rem2) = input.split_at(diff);
        debug_assert_eq!(rem, rem2);
        Ok((consumed, rem))
    }
}

/// A parser that fails if any input remains after it runs.
///
/// Constructed via the [`Parser::strict`] method.
pub struct Strict<P>(P);

impl<P: Parser> Parser for Strict<P> {
    type Output<'i> = P::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        let (res, rem) = self.0.parse(input)?;
        if rem.is_empty() {
            Ok((res, rem))
        } else {
            Err(Error::TrailingInput)
        }
    }
}

pub struct Func<F>(F);

impl<F, T> Parser for Func<F>
where
    F: for<'i> Fn(&'i str) -> IResult<'i, T>,
{
    type Output<'i> = T;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        (self.0)(input)
    }
}

pub fn mk_parser<F, T>(f: F) -> Func<F>
where
    F: for<'i> Fn(&'i str) -> IResult<'i, T>,
{
    Func(f)
}

impl<F, P> Parser for F
where
    F: Fn() -> P,
    P: Parser,
{
    type Output<'i> = P::Output<'i>;

    fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
        (self)().parse(input)
    }
}

/// A parser that extracts exactly one specific character from the input.
///
/// The parser will fail if the input is empty, or the next character is not `c`.
///
/// Outputs the extracted character.
pub fn one(c: char) -> impl for<'i> Parser<Output<'i> = char> {
    mk_parser(move |input| {
        let sym = input.chars().next().ok_or(Error::InputExhausted)?;
        if sym == c {
            let (_, rem) = split_at_chars(input, 1);
            Ok((sym, rem))
        } else {
            Err(Error::UnexpectedChar(sym))
        }
    })
}

/// A parser that extracts exactly the given sequence of characters from the input.
///
/// The parser will fail if the given string is not a prefix of the input.
///
/// Returns the extracted string slice from the input.
pub fn literal<'s>(lit: &'s str) -> impl for<'i> Parser<Output<'i> = &'i str> + 's {
    struct Literal<'s> {
        lit: &'s str,
    }

    impl Parser for Literal<'_> {
        type Output<'i> = &'i str;

        fn parse<'i>(&self, input: &'i str) -> IResult<'i, Self::Output<'i>> {
            let mut input_chars = input.chars();
            for expected in self.lit.chars() {
                if let Some(c) = input_chars.next() {
                    if c != expected {
                        return Err(Error::UnexpectedChar(c));
                    }
                } else {
                    return Err(Error::InputExhausted);
                }
            }

            Ok(input.split_at(self.lit.len()))
        }
    }

    Literal { lit }
}

/// A parser that extracts a single character from the input, as long as it passes the given
/// predicate.
///
/// The parser will fail if the input is empty, or if the first character does not pass the given
/// predicate.
///
/// Returns the extracted character.
pub fn predicate<F>(pred: F) -> impl for<'i> Parser<Output<'i> = char>
where
    F: Fn(char) -> bool,
{
    mk_parser(move |input| {
        let sym = input.chars().next().ok_or(Error::InputExhausted)?;
        if pred(sym) {
            let (_, rem) = split_at_chars(input, 1);
            Ok((sym, rem))
        } else {
            Err(Error::UnexpectedChar(sym))
        }
    })
}

/// A parser that extracts as many characters as satsify the given predicate.
///
/// The parser will fail if no characters satisfy the predicate. Use [`Parser::optional`] to accept
/// the empty sequence.
///
/// This is equivalent to `precicate(pred).many().consumed()`.
pub fn extract_while<F>(pred: F) -> impl for<'i> Parser<Output<'i> = &'i str>
where
    F: Fn(char) -> bool,
{
    predicate(pred).many().consumed()
}

/// A parser that extracts characters until a character satisfies the given predicate.
///
/// The parser will fail if no characters fail to satisfy the predicate. Use [`Parser::optional`]
/// to accept the empty sequence.
///
/// This is equivalent to `extract_while(|c| !pred(c))`.
pub fn extract_until<F>(pred: F) -> impl for<'i> Parser<Output<'i> = &'i str>
where
    F: Fn(char) -> bool,
{
    extract_while(move |c| !pred(c))
}

/// A parser that skips over any whitespace in the input.
pub fn whitespace() -> impl for<'i> Parser<Output<'i> = ()> {
    extract_while(|c| c.is_whitespace()).optional().map(|_| ())
}

/// A parser that extracts an identifier from the input.
///
/// Identifiers are substrings that begin with an alphabetic character or `'_'`, and are composed
/// of alphanumeric characters and `'_'`.
///
/// This parser extracts as long an identifier as possible from the input.
pub fn identifier() -> impl for<'i> Parser<Output<'i> = &'i str> {
    predicate(|c| c.is_alphabetic() || c == '_')
        .sequence(extract_while(|c| c.is_alphanumeric() || c == '_').optional())
        .consumed()
}

/// A parser that extracts an integer of the given radix from the input.
///
/// This parser does not convert the extracted string into a native integer; use
/// [`Parser::map_from_str`] to do so.
pub fn integer(radix: u32) -> impl for<'i> Parser<Output<'i> = &'i str> {
    extract_while(move |c| c.is_digit(radix))
}

fn split_at_chars(input: &str, n: usize) -> (&str, &str) {
    if n == 0 || input.is_empty() {
        input.split_at(0)
    } else {
        let (i, c) = input.char_indices().take(n).last().unwrap();
        let len = c.len_utf8();
        input.split_at(i + len)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("early end of input")]
    InputExhausted,

    #[error("trailing input")]
    TrailingInput,

    #[error("unexpected character {0:?} in input")]
    UnexpectedChar(char),

    #[error(transparent)]
    Custom(Box<dyn std::error::Error + 'static>),
}

impl Error {
    pub fn custom<E: std::error::Error + 'static>(error: E) -> Self {
        Self::Custom(Box::new(error))
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub type IResult<'i, T> = Result<(T, &'i str)>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn one_successful() {
        let input = "heya";
        assert_eq!(one('h').parse(input).unwrap(), ('h', "eya"));
    }

    #[test]
    fn one_unsuccessful() {
        let input = "heya";
        assert!(matches!(
            one('x').parse(input).unwrap_err(),
            Error::UnexpectedChar('h')
        ));
    }

    #[test]
    fn literal_successful() {
        let input = "what's up";
        assert_eq!(literal("what").parse(input).unwrap(), ("what", "'s up"));
    }

    #[test]
    fn literal_unsuccessful() {
        let input = "what's up";
        assert!(matches!(
            literal("whats").parse(input).unwrap_err(),
            Error::UnexpectedChar('\'')
        ));
    }

    #[test]
    fn predicate_successful() {
        let input = "abcd1234";
        assert_eq!(
            predicate(|c| c.is_ascii_alphabetic()).parse(input).unwrap(),
            ('a', "bcd1234")
        );
    }

    #[test]
    fn predicate_unsuccessful() {
        let input = "abcd1234";
        assert!(matches!(
            predicate(|c| c.is_ascii_digit()).parse(input).unwrap_err(),
            Error::UnexpectedChar('a')
        ));
    }

    #[test]
    fn extract_while_successful() {
        let input = "abcd1234";
        assert_eq!(
            extract_while(|c| c.is_ascii_alphabetic())
                .parse(input)
                .unwrap(),
            ("abcd", "1234")
        );
    }

    #[test]
    fn extract_while_unsuccessful() {
        let input = "abcd1234";
        assert!(matches!(
            extract_while(|c| c.is_ascii_digit())
                .parse(input)
                .unwrap_err(),
            Error::UnexpectedChar('a')
        ));
    }

    #[test]
    fn extract_until_successful() {
        let input = "abcd1234";
        assert_eq!(
            extract_until(|c| c.is_ascii_digit()).parse(input).unwrap(),
            ("abcd", "1234")
        );
    }

    #[test]
    fn extract_until_unsuccessful() {
        let input = "abcd1234";
        assert!(matches!(
            extract_until(|c| c.is_ascii_alphabetic())
                .parse(input)
                .unwrap_err(),
            Error::UnexpectedChar('a'),
        ));
    }

    #[test]
    fn whitespace_nonempty() {
        let input = " \t\nlmao";
        assert_eq!(whitespace().parse(input).unwrap(), ((), "lmao"));
    }

    #[test]
    fn whitespace_empty() {
        let input = "whatever lol";
        assert_eq!(whitespace().parse(input).unwrap(), ((), input));
    }

    #[test]
    fn identifier_successful() {
        let input = "so_32, hey how bout it";
        assert_eq!(
            identifier().parse(input).unwrap(),
            ("so_32", ", hey how bout it")
        );
    }

    #[test]
    fn identifier_unsuccessful() {
        let input = "2 spoopy";
        assert!(matches!(
            identifier().parse(input).unwrap_err(),
            Error::UnexpectedChar('2')
        ));
    }

    #[test]
    fn integer_successful() {
        let input = "1412 and more";
        assert_eq!(integer(10).parse(input).unwrap(), ("1412", " and more"));

        let input = "deadbeef innit";
        assert_eq!(integer(16).parse(input).unwrap(), ("deadbeef", " innit"));
    }

    #[test]
    fn integer_unsuccessful() {
        let input = "deadbeef innit";
        assert!(matches!(
            integer(10).parse(input).unwrap_err(),
            Error::UnexpectedChar('d')
        ));
    }

    #[test]
    fn sequence_successful() {
        let input = "hey there nerd";
        let p = identifier();
        let q = whitespace().sequence(identifier()).second();
        let parser = p.sequence(q);

        assert_eq!(parser.parse(input).unwrap(), (("hey", "there"), " nerd"));
    }

    #[test]
    fn sequence_unsuccessful_first() {
        let input = "no";
        let parser = one('a').sequence(one('o'));
        assert!(matches!(
            parser.parse(input).unwrap_err(),
            Error::UnexpectedChar('n')
        ));
    }

    #[test]
    fn sequence_unsuccessful_second() {
        let input = "no";
        let parser = one('n').sequence(one('a'));
        assert!(matches!(
            parser.parse(input).unwrap_err(),
            Error::UnexpectedChar('o')
        ));
    }

    #[test]
    fn alternate_successful_first() {
        let input = "hey there";
        let parser = identifier().alternate(integer(10));
        assert_eq!(parser.parse(input).unwrap(), ("hey", " there"));
    }

    #[test]
    fn alternate_successful_second() {
        let input = "10 things";
        let parser = identifier().alternate(integer(10));
        assert_eq!(parser.parse(input).unwrap(), ("10", " things"));
    }

    #[test]
    fn alternate_unsuccessful() {
        let input = "nope";
        let parser = one('a').alternate(one('b'));
        assert!(matches!(
            parser.parse(input).unwrap_err(),
            Error::UnexpectedChar('n')
        ));
    }
}

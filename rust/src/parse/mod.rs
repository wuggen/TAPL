pub trait Parser<'i> {
    type Output;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output>;

    fn both<P: Parser<'i>>(self, other: P) -> Both<Self, P>
    where
        Self: Sized,
    {
        Both { p: self, q: other }
    }

    fn then<P: Parser<'i>>(self, other: P) -> Then<Self, P>
    where
        Self: Sized,
    {
        Then {
            inner: self.both(other),
        }
    }

    fn or<P: Parser<'i, Output = Self::Output>>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
    {
        Or { p: self, q: other }
    }

    fn map<F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { p: self, f }
    }

    fn repeat(self, n: usize) -> Repeat<Self>
    where
        Self: Sized,
    {
        Repeat { p: self, n }
    }

    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many(self)
    }

    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self)
    }
}

pub struct Both<P, Q> {
    p: P,
    q: Q,
}

impl<'i, P: Parser<'i>, Q: Parser<'i>> Parser<'i> for Both<P, Q> {
    type Output = (P::Output, Q::Output);

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        let (p_out, rem) = self.p.parse(input)?;
        let (q_out, rem) = self.q.parse(rem)?;
        let out = (p_out, q_out);
        Ok((out, rem))
    }
}

pub struct Then<P, Q> {
    inner: Both<P, Q>,
}

impl<'i, P: Parser<'i>, Q: Parser<'i>> Parser<'i> for Then<P, Q> {
    type Output = Q::Output;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        let ((_, out), rem) = self.inner.parse(input)?;
        Ok((out, rem))
    }
}

pub struct Or<P, Q> {
    p: P,
    q: Q,
}

impl<'i, P, Q> Parser<'i> for Or<P, Q>
where
    P: Parser,
    Q: Parser<Output = P::Output>,
{
    type Output = P::Output;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        self.p.parse(input).or_else(|_| self.q.parse(input))
    }
}

pub struct Map<P, F> {
    p: P,
    f: F,
}

impl<'i, P, F, T> Parser<'i> for Map<P, F>
where
    P: Parser,
    F: Fn(P::Output) -> T,
{
    type Output = T;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        let (res, rem) = self.p.parse(input)?;
        let out = (self.f)(res);
        Ok((out, rem))
    }
}

pub struct Repeat<P> {
    p: P,
    n: usize,
}

impl<'i, P: Parser<'i>> Parser<'i> for Repeat<P> {
    type Output = Vec<P::Output>;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
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

pub struct Many<P>(P);

impl<'i, P: Parser<'i>> Parser<'i> for Many<P> {
    type Output = Vec<P::Output>;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        let mut rem = input;
        let mut out = Vec::new();

        while let Ok((item, r)) = self.0.parse(rem) {
            rem = r;
            out.push(item);
        }

        Ok((out, rem))
    }
}

pub struct Optional<P>(P);

impl<'i, P: Parser<'i>> Parser<'i> for Optional<P> {
    type Output = Option<P::Output>;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        self.0
            .parse(input)
            .map(|(out, rem)| (Some(out), rem))
            .or(Ok((None, input)))
    }
}

impl<'i, F, T> Parser<'i> for F
where
    F: Fn(&'i str) -> IResult<'i, T>,
{
    type Output = T;

    fn parse(&self, input: &'i str) -> IResult<'i, Self::Output> {
        self(input)
    }
}

pub fn one<'i>(c: char) -> impl Parser<'i, Output = ()> {
    move |input: &'i str| -> IResult<'i, _> {
        let (i, sym) = input.char_indices().next().ok_or(Error::InputExhausted)?;
        if sym == c {
            let n = sym.len_utf8();
            let rem = input.split_at(i + n).1;
            Ok(((), rem))
        } else {
            Err(Error::UnexpectedChar(sym))
        }
    }
}

pub fn literal<'i>(lit: &str) -> impl Parser<'i, Output = &'i str> {
    todo!()
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("early end of input")]
    InputExhausted,

    #[error("unexpected character {0:?} in input")]
    UnexpectedChar(char),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type IResult<'i, T> = Result<(T, &'i str)>;

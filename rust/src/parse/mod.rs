use std::cell::{Ref, RefCell, RefMut};
use std::ops::{Deref, Range};

pub struct InputStream<T, I> {
    inner: RefCell<InputStreamInner<I>>,
    buffer: RefCell<Vec<T>>,
}

struct InputStreamInner<I> {
    input: I,
    cursors: Vec<u32>,
    position: usize,
}

impl<T, I> InputStream<T, I> {
    pub fn new(input: I) -> Self {
        let inner = RefCell::new(InputStreamInner {
            input,
            cursors: Vec::new(),
            position: 0,
        });
        let buffer = RefCell::new(Vec::new());

        Self { inner, buffer }
    }

    fn buffer_mut(&self) -> Result<RefMut<Vec<T>>> {
        self.buffer
            .try_borrow_mut()
            .map_err(|_| Error::StreamUnavailable)
    }
}

impl<T, I: Iterator<Item = Result<T>>> InputStream<T, I> {
    pub fn cursor(&self) -> Cursor<T, I> {
        let pos = self.inner.borrow().position;
        self.cursor_at(pos)
    }

    fn cursor_at(&self, position: usize) -> Cursor<T, I> {
        let mut inner = self.inner.borrow_mut();
        let offset = position - inner.position;

        if inner.cursors.len() <= offset {
            inner.cursors.resize(offset + 1, 0);
        }

        inner.cursors[offset] += 1;

        Cursor {
            stream: self,
            position,
        }
    }

    /// Ensure that the buffer contains all symbols from the current stream position (inclusive) to
    /// the given stream position (exclusive).
    ///
    /// If sufficient symbols are already buffered, this is a no-op.
    fn buffer_to_position(&self, position: usize) -> Result<()> {
        let mut inner = self.inner.borrow_mut();
        debug_assert!(position >= inner.position);

        let mut buffer = self.buffer_mut()?;

        let n = position - inner.position;
        buffer.reserve(n);

        for _ in 0..n {
            if let Some(res) = inner.input.next() {
                buffer.push(res?);
            } else {
                return Err(Error::InputExhausted);
            }
        }

        Ok(())
    }

    /// Drop the first at most `n` symbols from the buffer.
    ///
    /// If there are fewer than `n` symbols in the buffer, this will advance the input iterator by
    /// the remaining amount, and discard the yielded items.
    ///
    /// Additionally, this will drop the first at most `n` cursors from the cursor buffer. No
    /// checks are made to ensure that they are zero.
    ///
    /// Advances the stream position by the lesser of `n` and the remaining symbols in the input.
    fn drop_n(
        n: usize,
        buffer: &mut RefMut<Vec<T>>,
        inner: &mut RefMut<InputStreamInner<I>>,
    ) {
        let drained = usize::min(n, buffer.len());
        let to_drop = n - drained;

        buffer.drain(0..drained);
        let dropped = (&mut inner.input).skip(to_drop).count();

        let cursors_drained = usize::min(n, inner.cursors.len());
        inner.cursors.drain(0..cursors_drained);

        inner.position += drained + dropped;
    }

    /// Drop a cursor at the given stream position.
    ///
    /// This decrements the number of cursors at that position. If this results in there being at
    /// least one symbol at the current stream position with no cursors:
    ///
    /// - If there is at least one other cursor to this stream, all symbols up to the next cursor
    ///   will be dropped, including any that have not yet been buffered, advancing the stream
    ///   position to the position of the next cursor.
    /// - If there are no more cursors, the buffer and the stream position will remain unchanged.
    fn drop_cursor(&self, position: usize) {
        let mut inner = self.inner.borrow_mut();
        debug_assert!(position >= inner.position);
        let offset = position - inner.position;
        debug_assert!(offset < inner.cursors.len());
        debug_assert!(inner.cursors[offset] > 0);

        inner.cursors[offset] -= 1;

        if let Ok(mut buffer) = self.buffer_mut() {
            let n = inner
                .cursors
                .iter()
                .position(|n| *n > 0)
                .unwrap_or_else(|| inner.cursors.len());
            Self::drop_n(n, &mut buffer, &mut inner);
        }
    }
}

pub struct Cursor<'i, T, I: Iterator<Item = Result<T>>> {
    stream: &'i InputStream<T, I>,
    position: usize,
}

impl<'i, T, I: Iterator<Item = Result<T>>> Drop for Cursor<'i, T, I> {
    fn drop(&mut self) {
        self.stream.drop_cursor(self.position);
    }
}

pub struct Lookahead<'i, T> {
    buffer: Ref<'i, Vec<T>>,
    range: Range<usize>,
}

impl<'i, T> Deref for Lookahead<'i, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.buffer[self.range.clone()]
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("the input buffer is currently borrowed; cannot modify")]
    StreamUnavailable,

    #[error("early end of input")]
    InputExhausted,
}

pub type Result<T> = std::result::Result<T, Error>;

//use std::collections::VecDeque;
//use std::ops::RangeInclusive;
//
//mod internal;
//
//pub struct ParserInput<T> {
//    input: Box<dyn Iterator<Item = T>>,
//    lookahead: VecDeque<T>,
//}
//
//impl<T> ParserInput<T> {
//    pub fn new<I>(input: I) -> Self
//    where
//        I: IntoIterator<Item = T>,
//        I::IntoIter: 'static,
//    {
//        let input = Box::new(input.into_iter().fuse());
//        let lookahead = VecDeque::new();
//        Self { input, lookahead }
//    }
//
//    /// Look ahead `n` symbols in the input.
//    ///
//    /// This does not advance the input. Subsequent calls to `lookahead_n` without intervening
//    /// advances will contain the returned slice as as a prefix (if called with greater `n`) or
//    /// will be prefixes of the returned slice (if called with lesser `n`).
//    ///
//    /// Returns a slice of length at most `n`. The length will be shorter if the remaining input is
//    /// shorter than `n`.
//    pub fn lookahead_n(&mut self, n: usize) -> &[T] {
//        if self.lookahead.len() < n {
//            self.lookahead_additional(n - self.lookahead.len());
//        }
//
//        let actual = usize::min(n, self.lookahead.len());
//        &self.lookahead.make_contiguous()[0..actual]
//    }
//
//    /// Advance the input `n` symbols.
//    ///
//    /// Returns the symbols advanced over. This may be less than `n` if the remaining input is
//    /// shorter than `n`.
//    pub fn advance_n(&mut self, n: usize) -> Vec<T> {
//        let already_buffered = usize::min(n, self.lookahead.len());
//        let additional_needed = n - already_buffered;
//
//        let mut syms: Vec<T> = self.lookahead.drain(0..already_buffered).collect();
//
//        for _ in 0..additional_needed {
//            if let Some(sym) = self.input.next() {
//                syms.push(sym);
//            } else {
//                break;
//            }
//        }
//
//        syms
//    }
//
//    /// Look ahead at the next symbol in the input.
//    ///
//    /// This does not advance the input. Subsequent calls to `lookahead` without intervening
//    /// advances will return the same symbol.
//    pub fn lookahead(&mut self) -> Option<&T> {
//        if self.lookahead.is_empty() {
//            self.lookahead_additional(1);
//        }
//        self.lookahead.get(0)
//    }
//
//    /// Advance the input one symbol.
//    pub fn advance(&mut self) -> Option<T> {
//        if self.lookahead.is_empty() {
//            self.input.next()
//        } else {
//            self.lookahead.pop_front()
//        }
//    }
//}
//
//impl<T> ParserInput<T>
//where
//    T: Eq,
//{
//    /// Attempts to advance over one symbol equal to an expected symbol.
//    ///
//    /// If the input is empty, or if the next symbol is not equal to the expected symbol, returns
//    /// `None` and does not advance the input. Otherwise, advances the input by one symbol and
//    /// returns the extracted symbol.
//    pub fn one(&mut self, expected: &T) -> Option<T> {
//        if let Some(t) = self.lookahead() {
//            if t == expected {
//                self.advance()
//            } else {
//                None
//            }
//        } else {
//            None
//        }
//    }
//
//    /// Attempts to advance over as many symbols equal to the expected symbol as possible.
//    ///
//    /// Advances the input by as many symbols equal to `expected` are encountered. Returns a vec of
//    /// all extracted symbols.
//    pub fn many(&mut self, expected: &T) -> Vec<T> {
//        let mut syms = Vec::new();
//        while let Some(t) = self.lookahead() {
//            if t == expected {
//                syms.push(self.advance().unwrap());
//            } else {
//                break;
//            }
//        }
//        syms
//    }
//}
//
//impl<T> ParserInput<T>
//where
//    T: Ord,
//{
//    /// Attempts to advance over one symbol in the given range.
//    ///
//    /// If the input is empty, or if the next symbol is not within the given range, returns `None`
//    /// and does not advance the input. Otherwise, advances the input by one symbol and returns the
//    /// extracted symbol.
//    pub fn one_in_range(&mut self, expected: RangeInclusive<T>) -> Option<T> {
//        if let Some(t) = self.lookahead() {
//            todo!()
//        } else {
//            None
//        }
//    }
//}
//
//impl<T> ParserInput<T> {
//    /// Advance the input by `amount` items, appending them to the lookahead buffer.
//    ///
//    /// Returns the number of items actually appended. This will be less than `amount` if the input
//    /// contains fewer than `amount` additional symbols.
//    fn lookahead_additional(&mut self, amount: usize) -> usize {
//        for n in 0..amount {
//            if let Some(item) = self.input.next() {
//                self.lookahead.push_back(item);
//            } else {
//                return n;
//            }
//        }
//
//        amount
//    }
//}

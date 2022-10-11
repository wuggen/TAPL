// Language:
//
// term ::=
//      applist
//      \ident.term
//      if term then term else term
//
// applist ::=
//      atom applistrest
//
// applistrest ::=
//      ε
//      atom applistrest
//
// atom ::=
//      ident
//      num
//      (term)

use std::ops::ControlFlow::{self, *};

use crate::intern::Text;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tok {
    Lambda,
    Dot,
    If,
    Then,
    Else,
    OParen,
    CParen,
    Num(u32),
    Ident(Text),
}

pub struct Lexer<C> {
    chars: C,
    lookahead: Option<char>,
    buffer: String,
    state: LexerState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerState {
    Start,
    MaybeLineComment,
    LineComment,
    Number,
    Ident,
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("unexpected character {0:?} in input")]
    UnexpectedCharacter(char),

    #[error("trailing characters in input")]
    TrailingInput,
}

impl<C> Lexer<C>
where
    C: Iterator<Item = char>,
{
    pub fn new(mut chars: C) -> Self {
        let lookahead = chars.next();
        let buffer = String::new();
        let state = LexerState::Start;
        Self {
            chars,
            lookahead,
            buffer,
            state,
        }
    }

    fn advance(&mut self) {
        self.lookahead = self.chars.next();
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.lookahead {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan(&mut self) -> Result<Option<Tok>, ParserError> {
        loop {
            if let Break(res) = match self.state {
                LexerState::Start => self.start(),
                LexerState::MaybeLineComment => self.maybe_line_comment(),
                LexerState::LineComment => self.line_comment(),
                LexerState::Number => self.number(),
                LexerState::Ident => self.ident(),
            } {
                break res;
            }
        }
    }

    fn start(&mut self) -> ControlFlow<Result<Option<Tok>, ParserError>> {
        debug_assert_eq!(self.state, LexerState::Start);
        self.skip_ws();

        match self.lookahead {
            None => Break(Ok(None)),

            Some('-') => {
                self.state = LexerState::MaybeLineComment;
                Continue(())
            }

            Some('\\') => {
                self.advance();
                Break(Ok(Some(Tok::Lambda)))
            }

            Some('.') => {
                self.advance();
                Break(Ok(Some(Tok::Dot)))
            }

            Some('(') => {
                self.advance();
                Break(Ok(Some(Tok::OParen)))
            }

            Some(')') => {
                self.advance();
                Break(Ok(Some(Tok::CParen)))
            }

            Some(c) if c.is_ascii_digit() => {
                self.state = LexerState::Number;
                Continue(())
            }

            Some(c) if c.is_alphabetic() || c == '_' => {
                self.state = LexerState::Ident;
                Continue(())
            }

            Some(c) => Break(Err(ParserError::UnexpectedCharacter(c))),
        }
    }

    fn maybe_line_comment(&mut self) -> ControlFlow<Result<Option<Tok>, ParserError>> {
        debug_assert_eq!(self.lookahead, Some('-'));
        self.advance();
        match self.lookahead {
            None => Break(Err(ParserError::UnexpectedCharacter('_'))),
            Some('-') => {
                self.state = LexerState::LineComment;
                Continue(())
            }
            Some(_) => Break(Err(ParserError::UnexpectedCharacter('-'))),
        }
    }

    fn line_comment(&mut self) -> ControlFlow<Result<Option<Tok>, ParserError>> {
        while let Some(c) = self.lookahead {
            if c == '\n' {
                break;
            } else {
                self.advance();
            }
        }

        self.state = LexerState::Start;
        Continue(())
    }

    fn number(&mut self) -> ControlFlow<Result<Option<Tok>, ParserError>> {
        debug_assert_eq!(self.state, LexerState::Number);
        debug_assert!(matches!(self.lookahead, Some(c) if c.is_ascii_digit()));

        while let Some(c) = self.lookahead {
            if c.is_ascii_digit() {
                self.buffer.push(c);
                self.advance();
            } else {
                break;
            }
        }

        self.state = LexerState::Start;
        let val = u32::from_str_radix(&self.buffer, 10).unwrap();
        self.buffer.clear();
        Break(Ok(Some(Tok::Num(val))))
    }

    fn ident(&mut self) -> ControlFlow<Result<Option<Tok>, ParserError>> {
        debug_assert_eq!(self.state, LexerState::Ident);
        debug_assert!(matches!(self.lookahead, Some(c) if c.is_alphabetic() || c == '_'));

        while let Some(c) = self.lookahead {
            if c.is_alphanumeric() || c == '_' || c == '\'' {
                self.buffer.push(c);
                self.advance();
            } else {
                break;
            }
        }

        self.state = LexerState::Start;

        let tok = match self.buffer.as_ref() {
            "if" => Tok::If,
            "then" => Tok::Then,
            "else" => Tok::Else,
            s => {
                let val = Text::from(s);
                Tok::Ident(val)
            }
        };

        self.buffer.clear();
        Break(Ok(Some(tok)))
    }
}

impl<C> Iterator for Lexer<C>
where
    C: Iterator<Item = char>,
{
    type Item = Result<Tok, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan().transpose()
    }
}

pub struct Parser<C> {
    lexer: Lexer<C>,
}

#[allow(dead_code)]
impl<C> Parser<C>
where
    C: Iterator<Item = char>,
{
    pub fn new(chars: C) -> Self {
        let lexer = Lexer::new(chars);
        Self { lexer }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_lexer() {
        use Tok::*;
        let input = r#"\.if then else () 1412 heya_Hey'hi"#;
        let expected = &[
            Lambda,
            Dot,
            If,
            Then,
            Else,
            OParen,
            CParen,
            Num(1412),
            Ident(Text::from("heya_Hey'hi")),
        ];

        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_, _>>().unwrap();
        assert_eq!(toks.as_slice(), expected);
    }

    #[test]
    fn multiline_lexer() {
        use Tok::*;
        let input = r#"
if a then b else c' -- Some kinda test or something I guess lmao
\d.iszero (succ d) baybee
"#;
        let expected = &[
            If,
            Ident(Text::from("a")),
            Then,
            Ident(Text::from("b")),
            Else,
            Ident(Text::from("c'")),
            Lambda,
            Ident(Text::from("d")),
            Dot,
            Ident(Text::from("iszero")),
            OParen,
            Ident(Text::from("succ")),
            Ident(Text::from("d")),
            CParen,
            Ident(Text::from("baybee")),
        ];

        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_, _>>().unwrap();
        assert_eq!(toks.as_slice(), expected);
    }

    #[test]
    fn comment_eof_lexer() {
        use Tok::*;
        let input = "hey --now that's a funny thing innit";
        let expected = &[Ident(Text::from("hey"))];

        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_, _>>().unwrap();
        assert_eq!(toks.as_slice(), expected);
    }

    #[test]
    fn empty_input_lexer() {
        let toks: Vec<_> = Lexer::new("".chars()).collect::<Result<_, _>>().unwrap();
        assert_eq!(toks.as_slice(), &[]);
    }

    #[test]
    fn all_comments_lexer() {
        let input = r#"-- Lmao hey look at this
-- all comments innit
-- Nothing but comments as far as I can see"#;
        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_, _>>().unwrap();
        assert_eq!(toks.as_slice(), &[]);
    }

    #[test]
    fn incomplete_comment_lexer() {
        let input = r#"\what that .yeah - hey"#;
        let err = Lexer::new(input.chars())
            .collect::<Result<Vec<_>, _>>()
            .unwrap_err();
        println!("{err}");
        assert!(matches!(err, ParserError::UnexpectedCharacter('-')));
    }
}

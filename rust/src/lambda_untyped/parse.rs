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

//use std::num::ParseIntError;

use crate::intern::Text;
use crate::parse::prelude::*;

use super::syntax::NamedTerm;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tok {
    Lambda,
    Dot,
    If,
    Then,
    Else,
    OParen,
    CParen,
    Semicolon,
    Num(u32),
    Ident(Text),
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '\''
}

fn is_ident_cont(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

fn term_from_int(n: u32) -> NamedTerm {
    let mut term = NamedTerm::zero();

    for _ in 0..n {
        term = NamedTerm::succ(term);
    }

    term
}

//fn ident() -> impl for<'i> Parser<Output<'i> = Text> {
//    whitespace().seq_second(
//        predicate(is_ident_start)
//            .sequence(extract_while(is_ident_cont).optional())
//            .consumed()
//            .map(Text::new),
//    )
//}
//
//fn var_or_const() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    ident().map(|t| match t.as_ref() {
//        "true" => NamedTerm::true_(),
//        "false" => NamedTerm::false_(),
//        name => NamedTerm::var(name),
//    })
//}
//
//fn num() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    whitespace().seq_second(integer(10).map_from_str().map(term_from_int))
//}
//
//fn atom() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    let paren_term = whitespace().seq_second(
//        one('(')
//            .seq_second(term)
//            .seq_first(whitespace())
//            .seq_first(one(')')),
//    );
//
//    paren_term.alternate(num()).alternate(var_or_const())
//}
//
//fn applist() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    atom().many().map(|terms| {
//        let mut terms = terms.into_iter();
//        let term = terms.next().unwrap();
//        terms.fold(term, |f, x| match &f {
//            NamedTerm::Var { name } => {
//                if name == "succ" {
//                    NamedTerm::succ(x)
//                } else if name == "pred" {
//                    NamedTerm::pred(x)
//                } else if name == "iszero" {
//                    NamedTerm::iszero(x)
//                } else {
//                    NamedTerm::app(f, x)
//                }
//            }
//            _ => NamedTerm::app(f, x),
//        })
//    })
//}
//
//fn abstr() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    whitespace().seq_second(
//        one('\\')
//            .alternate(one('λ'))
//            .sequence(whitespace())
//            .seq_second(ident())
//            .seq_first(whitespace().sequence(one('.')).sequence(whitespace()))
//            .sequence(term)
//            .map(|(v, t)| NamedTerm::binding(v, t)),
//    )
//}
//
//fn if_then_else() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    whitespace().seq_second(
//        literal("if")
//            .sequence(whitespace())
//            .seq_second(term)
//            .seq_first(
//                whitespace()
//                    .sequence(literal("then"))
//                    .sequence(whitespace()),
//            )
//            .sequence(term)
//            .seq_first(
//                whitespace()
//                    .sequence(literal("else"))
//                    .sequence(whitespace()),
//            )
//            .sequence(term)
//            .map(|((i, t), e)| NamedTerm::ite(i, t, e)),
//    )
//}
//
//fn term() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    applist().alternate(abstr()).alternate(if_then_else())
//}
//
//pub fn term_parser() -> impl for<'i> Parser<Output<'i> = NamedTerm> {
//    term()
//}

//pub struct Lexer<C> {
//    chars: C,
//    lookahead: Option<char>,
//    buffer: String,
//}
//
//#[derive(Debug, thiserror::Error)]
//pub enum ParserError {
//    #[error("unexpected character {0:?} in input")]
//    UnexpectedCharacter(char),
//
//    #[error("trailing characters in input")]
//    TrailingInput,
//
//    #[error("early end of input")]
//    Eof,
//
//    #[error("unexpected token {0:?} in input")]
//    UnexpectedToken(Tok),
//
//    #[error(transparent)]
//    IntegerOverflow { source: ParseIntError },
//}
//
//pub type Result<T> = std::result::Result<T, ParserError>;
//
//fn is_ident_start(c: char) -> bool {
//    c.is_alphabetic() || c == '_' || c == '\''
//}
//
//fn is_ident_cont(c: char) -> bool {
//    is_ident_start(c) || c.is_numeric()
//}
//
//impl<C> Lexer<C>
//where
//    C: Iterator<Item = char>,
//{
//    pub fn new(mut chars: C) -> Self {
//        let lookahead = chars.next();
//        let buffer = String::new();
//        Self {
//            chars,
//            lookahead,
//            buffer,
//        }
//    }
//
//    fn scan(&mut self) -> Result<Option<Tok>> {
//        self.skip_ws_and_comments()?;
//
//        if let Some(c) = self.lookahead {
//            match c {
//                '\\' | 'λ' => self.lambda(),
//                '.' => self.dot(),
//                '(' => self.oparen(),
//                ')' => self.cparen(),
//                ';' => self.semicolon(),
//                c if c.is_ascii_digit() => self.num(),
//                c if is_ident_start(c) => self.ident(),
//                c => Err(ParserError::UnexpectedCharacter(c)),
//            }
//        } else {
//            Ok(None)
//        }
//    }
//
//    fn advance(&mut self) {
//        self.lookahead = self.chars.next();
//    }
//
//    fn skip_ws(&mut self) {
//        while self.lookahead.map(|c| c.is_whitespace()).unwrap_or(false) {
//            self.advance();
//        }
//    }
//
//    fn skip_line_comment(&mut self) -> Result<()> {
//        if self.lookahead != Some('-') {
//            return Ok(());
//        }
//
//        self.advance();
//
//        if self.lookahead != Some('-') {
//            return Err(ParserError::UnexpectedCharacter('-'));
//        }
//
//        while !matches!(self.lookahead, Some('\n') | None) {
//            self.advance();
//        }
//
//        Ok(())
//    }
//
//    fn skip_ws_and_comments(&mut self) -> Result<()> {
//        while matches!(self.lookahead, Some(c) if c == '-' || c.is_whitespace()) {
//            self.skip_ws();
//            self.skip_line_comment()?;
//        }
//
//        Ok(())
//    }
//
//    fn single_char(&mut self, tok: Tok) -> Result<Option<Tok>> {
//        self.advance();
//        Ok(Some(tok))
//    }
//
//    fn lambda(&mut self) -> Result<Option<Tok>> {
//        self.single_char(Tok::Lambda)
//    }
//
//    fn dot(&mut self) -> Result<Option<Tok>> {
//        self.single_char(Tok::Dot)
//    }
//
//    fn oparen(&mut self) -> Result<Option<Tok>> {
//        self.single_char(Tok::OParen)
//    }
//
//    fn cparen(&mut self) -> Result<Option<Tok>> {
//        self.single_char(Tok::CParen)
//    }
//
//    fn semicolon(&mut self) -> Result<Option<Tok>> {
//        self.single_char(Tok::Semicolon)
//    }
//
//    fn ident(&mut self) -> Result<Option<Tok>> {
//        debug_assert!(matches!(self.lookahead, Some(c) if is_ident_start(c)));
//
//        self.buffer.clear();
//        while let Some(c) = self.lookahead {
//            if is_ident_cont(c) {
//                self.buffer.push(c);
//                self.advance();
//            } else {
//                break;
//            }
//        }
//
//        match self.buffer.as_ref() {
//            "if" => Ok(Some(Tok::If)),
//            "then" => Ok(Some(Tok::Then)),
//            "else" => Ok(Some(Tok::Else)),
//            ident => Ok(Some(Tok::Ident(Text::new(ident)))),
//        }
//    }
//
//    fn num(&mut self) -> Result<Option<Tok>> {
//        debug_assert!(matches!(self.lookahead, Some(c) if c.is_ascii_digit()));
//
//        self.buffer.clear();
//        while let Some(c) = self.lookahead {
//            if c.is_ascii_digit() {
//                self.buffer.push(c);
//                self.advance();
//            } else if c.is_alphanumeric() {
//                return Err(ParserError::UnexpectedCharacter(c));
//            } else {
//                break;
//            }
//        }
//
//        let n = self
//            .buffer
//            .parse::<u32>()
//            .map_err(|source| ParserError::IntegerOverflow { source })?;
//        Ok(Some(Tok::Num(n)))
//    }
//}
//
//impl<C> Iterator for Lexer<C>
//where
//    C: Iterator<Item = char>,
//{
//    type Item = Result<Tok>;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        self.scan().transpose()
//    }
//}
//
//pub struct Parser<C> {
//    lexer: Lexer<C>,
//    lookahead: Option<Tok>,
//}
//
//#[allow(dead_code)]
//impl<C> Parser<C>
//where
//    C: Iterator<Item = char>,
//{
//    pub fn new(chars: C) -> Result<Self> {
//        let mut lexer = Lexer::new(chars);
//        let lookahead = lexer.next().transpose()?;
//        Ok(Self { lexer, lookahead })
//    }
//
//    fn advance(&mut self) -> Result<()> {
//        self.lookahead = self.lexer.next().transpose()?;
//        Ok(())
//    }
//
//    fn match_one(&mut self, expected: Tok) -> Result<()> {
//        if let Some(tok) = self.lookahead.clone() {
//            if tok != expected {
//                Err(ParserError::UnexpectedToken(tok))
//            } else {
//                self.advance()?;
//                Ok(())
//            }
//        } else {
//            Err(ParserError::Eof)
//        }
//    }
//
//    fn term(&mut self) -> Result<Option<NamedTerm>> {
//        match self.lookahead.clone() {
//            None => Ok(None),
//            Some(Tok::Lambda) => self.abstraction(),
//            Some(Tok::If) => self.if_then_else(),
//            Some(Tok::Ident(_) | Tok::Num(_) | Tok::OParen) => self.applist(),
//            Some(tok) => Err(ParserError::UnexpectedToken(tok)),
//        }
//    }
//
//    fn abstraction(&mut self) -> Result<Option<NamedTerm>> {
//        todo!()
//    }
//
//    fn if_then_else(&mut self) -> Result<Option<NamedTerm>> {
//        todo!()
//    }
//
//    fn applist(&mut self) -> Result<Option<NamedTerm>> {
//        todo!()
//    }
//
//    fn atom(&mut self) -> Result<Option<NamedTerm>> {
//        todo!()
//    }
//}
//
//#[cfg(test)]
//mod test {
//    use super::*;
//
//    #[test]
//    fn basic_lexer() {
//        use Tok::*;
//        let input = r#"\.if then else () 1412 heya_Hey'hi"#;
//        let expected = &[
//            Lambda,
//            Dot,
//            If,
//            Then,
//            Else,
//            OParen,
//            CParen,
//            Num(1412),
//            Ident(Text::from("heya_Hey'hi")),
//        ];
//
//        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_>>().unwrap();
//        assert_eq!(toks.as_slice(), expected);
//    }
//
//    #[test]
//    fn multiline_lexer() {
//        use Tok::*;
//        let input = r#"
//if a then b else c' -- Some kinda test or something I guess lmao
//\d.iszero (succ d) baybee
//"#;
//        let expected = &[
//            If,
//            Ident(Text::from("a")),
//            Then,
//            Ident(Text::from("b")),
//            Else,
//            Ident(Text::from("c'")),
//            Lambda,
//            Ident(Text::from("d")),
//            Dot,
//            Ident(Text::from("iszero")),
//            OParen,
//            Ident(Text::from("succ")),
//            Ident(Text::from("d")),
//            CParen,
//            Ident(Text::from("baybee")),
//        ];
//
//        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_>>().unwrap();
//        assert_eq!(toks.as_slice(), expected);
//    }
//
//    #[test]
//    fn comment_eof_lexer() {
//        use Tok::*;
//        let input = "hey --now that's a funny thing innit";
//        let expected = &[Ident(Text::from("hey"))];
//
//        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_>>().unwrap();
//        assert_eq!(toks.as_slice(), expected);
//    }
//
//    #[test]
//    fn empty_input_lexer() {
//        let toks: Vec<_> = Lexer::new("".chars()).collect::<Result<_>>().unwrap();
//        assert_eq!(toks.as_slice(), &[]);
//    }
//
//    #[test]
//    fn all_comments_lexer() {
//        let input = r#"-- Lmao hey look at this
//-- all comments innit
//-- Nothing but comments as far as I can see"#;
//        let toks: Vec<_> = Lexer::new(input.chars()).collect::<Result<_>>().unwrap();
//        assert_eq!(toks.as_slice(), &[]);
//    }
//
//    #[test]
//    fn incomplete_comment_lexer() {
//        let input = r#"\what that .yeah - hey"#;
//        let err = Lexer::new(input.chars())
//            .collect::<Result<Vec<_>>>()
//            .unwrap_err();
//        println!("{err}");
//        assert!(matches!(err, ParserError::UnexpectedCharacter('-')));
//    }
//}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ident_parser() {
        let input = "horble dorble";
        let (text, _) = ident.parse(input).unwrap();
        println!("Parsed ident {}", text);
        assert_eq!(text, "horble");
    }
}

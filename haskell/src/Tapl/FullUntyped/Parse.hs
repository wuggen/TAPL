{-# LANGUAGE NamedFieldPuns #-}

-- | Parsers for Tapl.FullUntyped
module Tapl.FullUntyped.Parse
  ( parseNamedTerm
  , termParser
  ) where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

import Tapl.FullUntyped.Term

parseNamedTerm :: String -> Either ParseError NamedTerm
parseNamedTerm = parse term' ""
  where
    term' = do
        tspace
        t <- term
        eof
        return t

--parseTerm :: NamingContext -> String -> Either

termLangDef :: LanguageDef u
termLangDef = emptyDef {
    commentLine = "--",
    identStart = letter <|> oneOf "_'",
    identLetter = alphaNum <|> oneOf "_'",
    reservedNames =
      [ "if"
      , "then"
      , "else"
      , "succ"
      , "pred"
      , "iszero"
      , "true"
      , "false"
      ]
  }

TokenParser
  { identifier = tident
  , reserved = treserved
  , natural = tnat
  , symbol = tsym
  , whiteSpace = tspace
  , parens = tparens
  , lexeme = tlexeme
  } = makeTokenParser termLangDef

termParser = term

atomSym :: Parsec String u AtomSym
atomSym = aTrue <|> aFalse
  where
    aTrue = treserved "true" >> return ATrue
    aFalse = treserved "false" >> return AFalse

atom :: Parsec String u NamedTerm
atom = aNat <|> try aVar <|> (TAtom <$> atomSym)
  where
    aNat = natTerm <$> tnat
    aVar = TVar <$> tident

ite :: Parsec String u NamedTerm
ite = do
    treserved "if"
    t1 <- group
    treserved "then"
    t2 <- group
    treserved "else"
    t3 <- iteGroup
    return $ TIf t1 t2 t3

group :: Parsec String u NamedTerm
group = tparens term <|> atom

unaryGroup :: Parsec String u NamedTerm
unaryGroup = try unary <|> group

iteGroup :: Parsec String u NamedTerm
iteGroup = try ite <|> group

unarySym :: Parsec String u UnarySym
unarySym = uSucc <|> uPred <|> uIsZero
  where
    uSucc = treserved "succ" >> return USucc
    uPred = treserved "pred" >> return UPred
    uIsZero = treserved "iszero" >> return UIsZero

unary :: Parsec String u NamedTerm
unary = TUnary <$> unarySym <*> group

applist :: Parsec String u NamedTerm
applist = do
    lst <- al
    return $ foldl1 TApp lst
  where
    al = (:) <$> unaryGroup <*> alrest
    alrest = try ((:) <$> group <*> alrest)
         <|> return []

compound :: Parsec String u NamedTerm
compound = try ite <|> cBind <|> applist
  where
    cBind = do
        tlexeme $ char '\\'
        v <- tident
        tlexeme $ char '.'
        t <- term
        return $ TBind v t

term :: Parsec String u NamedTerm
term = try unary <|> compound
